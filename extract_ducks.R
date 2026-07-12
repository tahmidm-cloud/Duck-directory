# ESPNcricinfo Test Match Maidens Extractor
#
# Source:
# https://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;template=results;type=bowling;view=match
#
# This extracts match-by-match bowling rows, then aggregates total maidens
# for each unique player.
#
# Creates:
#   output/test_maidens.csv
#   output/maidens_report.csv
#
# test_maidens.csv format:
#   cricinfo_id,name,maidens

required_packages <- c(
  "httr2",
  "xml2",
  "stringr"
)

install_if_missing <- function(packages) {
  missing <- packages[!packages %in% rownames(installed.packages())]

  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(required_packages)

library(httr2)
library(xml2)
library(stringr)

base_url <- "https://stats.espncricinfo.com/ci/engine/stats/index.html"

output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

build_url <- function(page, size = 200) {
  paste0(
    base_url,
    "?class=1",
    ";page=", page,
    ";size=", size,
    ";template=results",
    ";type=bowling",
    ";view=match"
  )
}

fetch_html <- function(url) {
  message("Reading: ", url)

  req <- request(url) |>
    req_user_agent(
      paste(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "AppleWebKit/537.36 (KHTML, like Gecko)",
        "Chrome/126.0 Safari/537.36"
      )
    ) |>
    req_headers(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
      `Accept-Language` = "en-US,en;q=0.9",
      Connection = "close"
    ) |>
    req_timeout(45) |>
    req_retry(max_tries = 3)

  resp <- req_perform(req)
  html <- resp_body_string(resp)

  html_lower <- tolower(html)

  if (grepl("access denied", html_lower, fixed = TRUE)) {
    stop("ESPNcricinfo returned Access Denied for: ", url)
  }

  if (grepl("this page is under maintenance", html_lower, fixed = TRUE)) {
    stop("ESPNcricinfo returned 'This Page is under maintenance' for: ", url)
  }

  html
}

clean_text <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

clean_player_name <- function(x) {
  x <- clean_text(x)
  x <- sub("\\s*\\([^)]*\\)\\s*$", "", x)
  clean_text(x)
}

extract_player_id <- function(href) {
  if (is.na(href) || href == "") {
    return(NA_character_)
  }

  id <- str_match(
    href,
    "/(?:player|cricketers)/(?:[^/]*-)?([0-9]+)(?:\\.html)?"
  )[, 2]

  if (is.na(id)) {
    id <- str_match(href, "([0-9]+)\\.html")[, 2]
  }

  id
}

parse_number_value <- function(x) {
  raw <- clean_text(x)
  raw <- gsub(",", "", raw)

  if (is.na(raw) || raw == "" || raw == "-") {
    return(0L)
  }

  value <- suppressWarnings(as.integer(raw))

  if (is.na(value)) {
    return(NA_integer_)
  }

  value
}

get_cell_texts <- function(row, tag) {
  cells <- xml_find_all(row, paste0(".//", tag))
  clean_text(xml_text(cells))
}

find_column_index <- function(header, possible_names) {
  idx <- which(header %in% possible_names)

  if (length(idx) == 0) {
    return(NA_integer_)
  }

  idx[1]
}

parse_one_table <- function(tbl) {
  header_rows <- xml_find_all(tbl, ".//tr[th]")

  if (length(header_rows) == 0) {
    return(NULL)
  }

  header <- NULL

  for (hr in header_rows) {
    h <- get_cell_texts(hr, "th")

    has_player <- any(h %in% c("Player", "Bowler"))
    has_maidens <- any(h %in% c("Mdns", "Maidens"))

    if (has_player && has_maidens) {
      header <- h
      break
    }
  }

  if (is.null(header)) {
    return(NULL)
  }

  player_index <- find_column_index(header, c("Player", "Bowler"))
  maidens_index <- find_column_index(header, c("Mdns", "Maidens"))

  if (is.na(player_index) || is.na(maidens_index)) {
    return(NULL)
  }

  data_rows <- xml_find_all(tbl, ".//tr[td]")

  output <- data.frame(
    cricinfo_id = character(),
    name = character(),
    maidens = integer(),
    stringsAsFactors = FALSE
  )

  for (row in data_rows) {
    cells <- xml_find_all(row, ".//td")
    texts <- clean_text(xml_text(cells))

    if (length(texts) < max(player_index, maidens_index)) {
      next
    }

    player_name <- clean_player_name(texts[player_index])
    maidens_value <- parse_number_value(texts[maidens_index])

    if (is.na(player_name) || player_name == "" || tolower(player_name) %in% c("player", "bowler")) {
      next
    }

    if (is.na(maidens_value)) {
      next
    }

    first_cell_link <- xml_find_first(cells[player_index], ".//a")
    href <- xml_attr(first_cell_link, "href")
    cricinfo_id <- extract_player_id(href)

    if (is.na(cricinfo_id) || cricinfo_id == "") {
      next
    }

    output <- rbind(
      output,
      data.frame(
        cricinfo_id = cricinfo_id,
        name = player_name,
        maidens = maidens_value,
        stringsAsFactors = FALSE
      )
    )
  }

  if (nrow(output) == 0) {
    return(NULL)
  }

  output
}

parse_stats_table <- function(html) {
  page <- read_html(html)

  tables <- xml_find_all(page, ".//table[contains(@class, 'engineTable')]")

  if (length(tables) == 0) {
    tables <- xml_find_all(page, ".//table")
  }

  for (tbl in tables) {
    parsed <- parse_one_table(tbl)

    if (!is.null(parsed) && nrow(parsed) > 0) {
      return(parsed)
    }
  }

  data.frame(
    cricinfo_id = character(),
    name = character(),
    maidens = integer(),
    stringsAsFactors = FALSE
  )
}

extract_all_match_rows <- function(size = 200, max_pages = 1000, sleep_seconds = 1) {
  message("")
  message("====================================")
  message("Extracting TEST match-by-match maidens")
  message("====================================")

  all_pages <- list()

  for (page_num in seq_len(max_pages)) {
    url <- build_url(page_num, size)

    html <- fetch_html(url)
    page_data <- parse_stats_table(html)

    if (nrow(page_data) == 0) {
      message("[test] no more rows found. Stopping.")
      break
    }

    all_pages[[length(all_pages) + 1]] <- page_data

    message("[test] page ", page_num, " match rows: ", nrow(page_data))

    if (nrow(page_data) < size) {
      message("[test] final page reached.")
      break
    }

    Sys.sleep(sleep_seconds)
  }

  if (length(all_pages) == 0) {
    stop("No match-by-match bowling rows extracted.")
  }

  do.call(rbind, all_pages)
}

aggregate_maidens_by_player <- function(match_rows) {
  unique_ids <- unique(match_rows$cricinfo_id)

  output <- data.frame(
    cricinfo_id = character(),
    name = character(),
    maidens = integer(),
    stringsAsFactors = FALSE
  )

  for (id in unique_ids) {
    player_rows <- match_rows[match_rows$cricinfo_id == id, ]

    player_name <- player_rows$name[1]
    total_maidens <- sum(player_rows$maidens, na.rm = TRUE)

    output <- rbind(
      output,
      data.frame(
        cricinfo_id = id,
        name = player_name,
        maidens = total_maidens,
        stringsAsFactors = FALSE
      )
    )
  }

  output <- output[order(-output$maidens, output$name), ]

  output[, c("cricinfo_id", "name", "maidens")]
}

match_rows <- extract_all_match_rows(
  size = 200,
  max_pages = 1000,
  sleep_seconds = 1
)

aggregated <- aggregate_maidens_by_player(match_rows)

maidens_file <- file.path(output_dir, "test_maidens.csv")
write.csv(aggregated, maidens_file, row.names = FALSE, na = "")

report <- data.frame(
  format = "test",
  cricinfo_class = 1,
  source_view = "match",
  total_match_rows = nrow(match_rows),
  unique_players = nrow(aggregated),
  non_zero_maidens_players = sum(aggregated$maidens > 0, na.rm = TRUE),
  zero_maidens_players = sum(aggregated$maidens == 0, na.rm = TRUE),
  output_file = maidens_file,
  stringsAsFactors = FALSE
)

report_file <- file.path(output_dir, "maidens_report.csv")
write.csv(report, report_file, row.names = FALSE, na = "")

message("")
message("====================================")
message("DONE")
message("====================================")
message("Created files:")
message("  ", maidens_file)
message("  ", report_file)
message("")

print(report)
