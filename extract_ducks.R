# ESPNcricinfo Ducks Extractor - FULL FIXED VERSION
#
# Creates:
#   output/t20_ducks.csv
#   output/test_ducks.csv
#   output/odi_ducks.csv
#   output/t20i_ducks.csv
#   output/ducks_report.csv
#
# Each ducks CSV format:
#   cricinfo_id,name,ducks
#
# Important fix:
#   ESPN shows "-" for players with zero ducks.
#   This script converts "-" to 0 instead of skipping the player.

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

formats <- data.frame(
  format = c("t20", "test", "odi", "t20i"),
  cricinfo_class = c(6, 1, 2, 3),
  stringsAsFactors = FALSE
)

output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

build_url <- function(class_id, page, size = 200) {
  paste0(
    base_url,
    "?class=", class_id,
    ";orderby=ducks",
    ";page=", page,
    ";size=", size,
    ";template=results",
    ";type=batting"
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

parse_duck_value <- function(x) {
  duck_raw <- clean_text(x)
  duck_raw <- gsub(",", "", duck_raw)

  if (is.na(duck_raw) || duck_raw == "" || duck_raw == "-") {
    return(0L)
  }

  duck_value <- suppressWarnings(as.integer(duck_raw))

  if (is.na(duck_value)) {
    return(NA_integer_)
  }

  duck_value
}

get_cell_texts <- function(row, tag) {
  cells <- xml_find_all(row, paste0(".//", tag))
  clean_text(xml_text(cells))
}

parse_one_table <- function(tbl) {
  header_rows <- xml_find_all(tbl, ".//tr[th]")

  if (length(header_rows) == 0) {
    return(NULL)
  }

  header <- NULL

  for (hr in header_rows) {
    h <- get_cell_texts(hr, "th")

    if ("Player" %in% h && any(h %in% c("0", "Ducks"))) {
      header <- h
      break
    }
  }

  if (is.null(header)) {
    return(NULL)
  }

  player_index <- which(header == "Player")[1]
  duck_index <- which(header %in% c("0", "Ducks"))[1]

  if (is.na(player_index) || is.na(duck_index)) {
    return(NULL)
  }

  data_rows <- xml_find_all(tbl, ".//tr[td]")

  output <- data.frame(
    cricinfo_id = character(),
    name = character(),
    ducks = integer(),
    stringsAsFactors = FALSE
  )

  for (row in data_rows) {
    cells <- xml_find_all(row, ".//td")
    texts <- clean_text(xml_text(cells))

    if (length(texts) < max(player_index, duck_index)) {
      next
    }

    player_name <- clean_player_name(texts[player_index])
    duck_value <- parse_duck_value(texts[duck_index])

    if (is.na(player_name) || player_name == "" || tolower(player_name) == "player") {
      next
    }

    if (is.na(duck_value)) {
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
        ducks = duck_value,
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
    ducks = integer(),
    stringsAsFactors = FALSE
  )
}

extract_one_format <- function(format_name, class_id, size = 200, max_pages = 500, sleep_seconds = 1) {
  message("")
  message("====================================")
  message("Extracting ", toupper(format_name), " ducks")
  message("====================================")

  all_pages <- list()

  for (page_num in seq_len(max_pages)) {
    url <- build_url(class_id, page_num, size)

    html <- fetch_html(url)
    page_data <- parse_stats_table(html)

    if (nrow(page_data) == 0) {
      message("[", format_name, "] no more rows found. Stopping.")
      break
    }

    all_pages[[length(all_pages) + 1]] <- page_data

    message("[", format_name, "] page ", page_num, " rows: ", nrow(page_data))

    if (nrow(page_data) < size) {
      message("[", format_name, "] final page reached.")
      break
    }

    Sys.sleep(sleep_seconds)
  }

  if (length(all_pages) == 0) {
    stop("No data extracted for format: ", format_name)
  }

  out <- do.call(rbind, all_pages)

  out <- out[!duplicated(out$cricinfo_id), ]

  out <- out[order(-out$ducks, out$name), ]

  out <- out[, c("cricinfo_id", "name", "ducks")]

  out_file <- file.path(output_dir, paste0(format_name, "_ducks.csv"))

  write.csv(out, out_file, row.names = FALSE, na = "")

  report_row <- data.frame(
    format = format_name,
    cricinfo_class = class_id,
    total_players = nrow(out),
    non_zero_ducks = sum(out$ducks > 0, na.rm = TRUE),
    zero_ducks = sum(out$ducks == 0, na.rm = TRUE),
    output_file = out_file,
    stringsAsFactors = FALSE
  )

  message("")
  message("[", format_name, "] saved: ", out_file)
  message("[", format_name, "] total players: ", report_row$total_players)
  message("[", format_name, "] non-zero ducks: ", report_row$non_zero_ducks)
  message("[", format_name, "] zero ducks: ", report_row$zero_ducks)

  list(
    data = out,
    report = report_row
  )
}

all_reports <- list()

for (i in seq_len(nrow(formats))) {
  result <- extract_one_format(
    format_name = formats$format[i],
    class_id = formats$cricinfo_class[i],
    size = 200,
    max_pages = 500,
    sleep_seconds = 1
  )

  all_reports[[length(all_reports) + 1]] <- result$report
}

report <- do.call(rbind, all_reports)

report_file <- file.path(output_dir, "ducks_report.csv")

write.csv(report, report_file, row.names = FALSE, na = "")

message("")
message("====================================")
message("DONE")
message("====================================")
message("Created files:")
message("  ", file.path(output_dir, "t20_ducks.csv"))
message("  ", file.path(output_dir, "test_ducks.csv"))
message("  ", file.path(output_dir, "odi_ducks.csv"))
message("  ", file.path(output_dir, "t20i_ducks.csv"))
message("  ", report_file)
message("")

print(report)
