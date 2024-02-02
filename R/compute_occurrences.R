#' Internal function to `compute()` for `type = "occurrences"`
#' @noRd
#' @keywords Internal
compute_occurrences <- function(.query){
  switch(pour("atlas", "region"),
         "United Kingdom" = compute_occurrences_uk(.query),
         "Global" = compute_occurrences_gbif(.query),
         compute_occurrences_la(.query))
}

#' Internal function to `compute()` for `type = "occurrences"` for UK
#' @noRd
#' @keywords Internal
compute_occurrences_uk <- function(.query){
  result <- c(.query,
              list(fields = extract_fields(.query)))
  class(result) <- "computed_query"
  result  
}

#' Internal function to `compute()` for `type = "occurrences"` for GBIF
#' Note: this has not been updated and is unlikely to work
#' @noRd
#' @keywords Internal
compute_occurrences_gbif <- function(.query){
  post_result <- query_API(.query) # returns an id
  status_code <- list(
    type = "data/occurrences",
    url = paste0("https://api.gbif.org/v1/occurrence/download/", 
                 post_result)) |>
    query_API() |>
    check_occurrence_response()
  result <- c(
    list(type = "data/occurrences"),
    status_code)
  class(result) <- "computed_query"
  return(result)
}

#' Internal function to `compute()` for `type = "occurrences"` for ALA
#' @noRd
#' @keywords Internal
compute_occurrences_la <- function(.query){
  # ping the API
  status_code <- query_API(.query) |>
    as.list() |>
    check_occurrence_response()
  if(pour("package", "verbose")){
    n_records <- status_code$total_records
    inform(glue("Request for {n_records} occurrences placed in queue"))
  }
  # return a useful object
  result <- c(
    list(type = "data/occurrences"),
    status_code, 
    list(fields = extract_fields(.query)))
  class(result) <- "computed_query"
  result
}

#' Internal function to get the `fields` vector from a url
#' @importFrom httr2 url_parse
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
extract_fields <- function(.query){
  .query |>
    pluck("url") |>
    url_parse() |>
    pluck("query", "fields") |>
    strsplit(split = ",") |>
    pluck(!!!list(1))
}
