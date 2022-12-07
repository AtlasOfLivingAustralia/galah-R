## GBIF defaults
# there is no API for GBIF fields, so best practice is to store as local 
# csvs and import as per above

library(readr) # import csvs straight to tibble
library(tibble) # generate tibbles
library(dplyr) # data manipulation
library(purrr) # extraction from lists
library(rvest) # web scraping assertions from gbif.org

# tibble of available fields: 
gbif_parameters_url <- "https://www.gbif.org/developer/occurrence#parameters"
data_raw <- read_html(gbif_parameters_url) |> 
  html_node("body")|> 
  html_nodes("table")

data_text <- data_raw |>
  pluck(7) |> # poor practice to hard-code this; checks may be needed
  html_nodes("tr") |> 
  html_text()

lapply(strsplit(data_text, "\n"), function(a){
  tibble(
    id = trimws(a[[1]]),
    description = trimws(paste(a[-1], collapse = "")))
}) |>
  bind_rows() |>
  slice(-1) |> # header row gets imported as a row by mistake
  mutate(type = "fields") |>
  filter(
    !grepl("^facet", id), 
    !(id %in% c("geometry", "geodistance", "q", "hl", "format"))) |>
  write_csv("./data-raw/gbif_fields.csv")


# tibble of assertions, scraped from gbif developer docs:
gbif_assertions_url <- "https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html"
data_raw <- read_html(gbif_assertions_url) |> html_node("body")

assertions_text <- data_raw |> 
  html_nodes("tr") |> 
  html_text()

assertions_list <- strsplit(
  assertions_text[2:which(grepl("^ZERO_COORDINATE", assertions_text))],
  "\n")

lapply(assertions_list, function(a){
  tibble(
    id = a[[1]],
    description = paste(a[-1], collapse = ""))
}) |>
  bind_rows() |>
  mutate(type = "assertions") |>
  write_csv("./data-raw/gbif_assertions.csv")