# Scrape NFL Salaries from Over the Cap
#
# The most useful function is `otc_load_and_cache_data_for_all_positions` which
# returns a data frame with all players and all salaries for all positions.
#
# https://overthecap.com/position/running-back/
library("rvest")
library("glue")
library("tidyverse")

positions = c(
  "quarterback",
  "running-back",
  "fullback",
  "wide-receiver",
  "tight-end",
  "left-tackle",
  "left-guard",
  "center",
  "right-guard",
  "right-tackle",
  "interior-defensive-line",
  "edge-rusher",
  "linebacker",
  "safety",
  "cornerback",
  "kicker",
  "punter",
  "long-snapper"
)

otc_url_for_position <- function(position) {
  glue("https://overthecap.com/position/{position}/")
}

otc_local_html_filename_for_position <- function(position) {
  glue("data/overthecap/{position}.html")
}

# Download file, cache to disk, and return HTML.
otc_download_file_for_position <- function(position) {
  local_filename <- otc_local_html_filename_for_position(position)

  if (!file.exists(local_filename)) {
    if (!dir.exists(dirname(local_filename))) {
      dir.create(dirname(local_filename))
    }
    url <- otc_url_for_position(position)
    download.file(url, local_filename)
  }
  return(read_html(local_filename))
}

otc_parse_data_for_position <- function(position) {
  file_contents <- otc_download_file_for_position(position)
  nodes <- html_nodes(file_contents, ".position-table")[[1]]
  data <- html_table(nodes, fill = TRUE)

  # Rename column for easier reference
  colnames(data)[5] <- "Avg.Year"

  data <- data %>% mutate(Position = position,
                          SalaryInDollars = parse_number(Avg.Year))

  if (nrow(data) > 0) {
    saveRDS(data, glue("data/overthecap/{position}.rds"))
  }

  return(data)
}

# Returns a single data frame with all positions and salaries.
otc_load_and_cache_data_for_all_positions <- function() {
  # Load and cache all data
  salaries <-
    positions %>%
    map(otc_parse_data_for_position) %>%
    reduce(bind_rows)
}
