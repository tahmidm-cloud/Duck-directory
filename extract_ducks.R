# ESPNcricinfo Ducks Extractor
# Creates 4 separate CSV files:
#   output/t20_ducks.csv
#   output/test_ducks.csv
#   output/odi_ducks.csv
#   output/t20i_ducks.csv
#
# Required output format:
#   cricinfo_id,name,ducks
#
# Also creates:
#   output/ducks_report.csv
#
# Report columns:
#   format,cricinfo_class,total_players,non_zero_ducks,zero_ducks,output_file

required_packages <- c(
  "httr2",
  "rvest",
  "dplyr",
  "purrr",
  "readr",
  "stringr",
  "tibble"
)

install_if_missing <- function(packages) {
  missing <- packages[!packages %in% rownames(installed.packages())]

  if (length(missing) > 0) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

install_if_missing(required_packages)

library(httr2)
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

base_url <- "https://stats.espncricinfo.com/ci/engine/stats/index.html"

formats <- tibble::tribble(
  ~format, ~cricinfo_class,
  "t20",   6,   # all T20 / Twenty20
  "test",  1,   # Test
  "odi",   2,   # ODI
  "t20i",  3    # T20I
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

  html_lower <- str_to_lower(html)

  if (str_detect(html_lower, "access denied")) {
    stop("ESPNcricinfo returned Access Denied for: ", url)
  }

  if (str_detect(html_lower, "this page is under maintenance")) {
    stop("ESPNcricinfo returned 'This Page is under maintenance' for: ", url)
  }

  html
}

extract_player_id <- function(href) {
  id <- str_match(href, "/(?:player|cricketers)/(\\d+)(?:\\.html)?")[, 2]

  ifelse(
    is.na(id),
    str_match(href, "/(\\d+)\\.html")[, 2],
    id
  )
}

clean_player_name <- function(name) {
  name |>
    str_replace("\\s*\\([^)]*\\)\\s*$", "") |>
    str_squish()
}

parse_stats_table <- function(html) {
  page <- read_html(html)

  tables <- page |> html_elements("table.engineTable")

  if (length(tables) == 0) {
    tables <- page |> html_elements("table")
  }

  for (tbl in tables) {
    table_df <- tryCatch(
      tbl |> html_table(fill = TRUE),
      error = function(e) NULL
    )

    if (is.null(table_df) || nrow(table_df) == 0) {
      next
    }

    names(table_df) <- names(table_df) |>
      str_replace_all("\\s+", " ") |>
      str_trim()

    if (!("Player" %in% names(table_df))) {
      next
    }

    duck_col <- names(table_df)[names(table_df) %in% c("0", "Ducks")]

    if (length(duck_col) == 0) {
      next
    }

    data_rows <- tbl |> html_elements("tr.data1, tr.data2")

    if (length(data_rows) == 0) {
      all_rows <- tbl |> html_elements("tr")

      data_rows <- all_rows[
        map_lgl(all_rows, function(row) {
          cells <- row |> html_elements("td")
          has_cells <- length(cells) > 0
          has_player_link <- length(row |> html_elements("td:first-child a")) > 0
          has_cells && has_player_link
        })
      ]
    }

    player_links <- data_rows |>
      map_chr(function(row) {
        link <- row |> html_element("td:first-child a")
        href <- link |> html_attr("href")

        ifelse(is.na(href), NA_character_, href)
      })

    player_ids <- extract_player_id(player_links)

    cleaned <- table_df |>
      filter(!is.na(Player)) |>
      mutate(Player = str_squish(as.character(Player))) |>
      filter(Player != "") |>
      filter(str_to_lower(Player) != "player") |>
      mutate(
        row_number_clean = row_number(),
        cricinfo_id = player_ids[row_number_clean],
        name = clean_player_name(Player),
        ducks = suppressWarnings(
          as.integer(str_replace_all(as.character(.data[[duck_col[1]]]), ",", ""))
        )
      ) |>
      select(cricinfo_id, name, ducks) |>
      filter(!is.na(cricinfo_id)) |>
      filter(!is.na(name), name != "") |>
      filter(!is.na(ducks)) |>
      distinct(cricinfo_id, .keep_all = TRUE) |>
      arrange(desc(ducks), name)

    if (nrow(cleaned) > 0) {
      return(cleaned)
    }
  }

  tibble(
    cricinfo_id = character(),
    name = character(),
    ducks = integer()
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

    message(
      "[", format_name, "] page ",
      page_num,
      " rows: ",
      nrow(page_data)
    )

    if (nrow(page_data) < size) {
      message("[", format_name, "] final page reached.")
      break
    }

    Sys.sleep(sleep_seconds)
  }

  if (length(all_pages) == 0) {
    stop("No data extracted for format: ", format_name)
  }

  out <- bind_rows(all_pages) |>
    distinct(cricinfo_id, .keep_all = TRUE) |>
    arrange(desc(ducks), name)

  out_file <- file.path(output_dir, paste0(format_name, "_ducks.csv"))

  readr::write_csv(out, out_file, na = "")

  report_row <- tibble(
    format = format_name,
    cricinfo_class = class_id,
    total_players = nrow(out),
    non_zero_ducks = sum(out$ducks > 0, na.rm = TRUE),
    zero_ducks = sum(out$ducks == 0, na.rm = TRUE),
    output_file = out_file
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
  format_name <- formats$format[i]
  class_id <- formats$cricinfo_class[i]

  result <- extract_one_format(
    format_name = format_name,
    class_id = class_id,
    size = 200,
    max_pages = 500,
    sleep_seconds = 1
  )

  all_reports[[length(all_reports) + 1]] <- result$report
}

report <- bind_rows(all_reports)

report_file <- file.path(output_dir, "ducks_report.csv")
readr::write_csv(report, report_file, na = "")

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