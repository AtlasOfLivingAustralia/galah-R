#' Internal function to `compute()` for `type = "occurrences"`
#' @noRd
#' @keywords Internal
compute_occurrences <- function(.query){
  switch(potions::pour("atlas", "region"),
         "Austria" = compute_occurrences_la_direct(.query),
         "United Kingdom" = compute_occurrences_la_direct(.query),
         "Global" = compute_occurrences_gbif(.query),
         compute_occurrences_la(.query))
}

#' Internal function to `compute()` for `type = "occurrences"` for 
#' atlases that have 'direct' downloads (i.e. no true `compute` stage)
#' @noRd
#' @keywords Internal
compute_occurrences_la_direct <- function(.query){
  c(.query,
    list(fields = extract_fields(.query))) |>
    structure(class = "computed_query")
}

#' Internal function to `compute()` for `type = "occurrences"` for GBIF
#' Note: this has not been updated and is unlikely to work
#' @noRd
#' @keywords Internal
compute_occurrences_gbif <- function(.query){
  post_result <- query_API(.query) # returns an id
  status_code <- list(
    type = "data/occurrences",
    url = glue::glue("https://api.gbif.org/v1/occurrence/download/{post_result}")) |>
    query_API() |>
    check_occurrence_response()
  c(list(type = "data/occurrences"),
    status_code) |>
    structure(class = "computed_query")
}

#' Internal function to `compute()` for `type = "occurrences"` for ALA
#' @noRd
#' @keywords Internal
compute_occurrences_la <- function(.query){
  # ping the API
  status_code <- query_API(.query) |>
    as.list() |>
    check_occurrence_response()
  if(potions::pour("package", "verbose")){
    n_records <- status_code$total_records
    cli::cli_par()
    if(!is.null(.query$authenticate)){
      cli::cli_text("Query sent including JWT token")
    }
    cli::cli_text("Request for {n_records} occurrences placed in queue")
    cli::cli_end()
  }
  # return a useful object
  c(list(type = "data/occurrences"),
    status_code, 
    list(fields = extract_fields(.query))) |>
  add_request(.query) |>
  structure(class = "computed_query")
}

#' Internal function to get the `fields` vector from a url
#' @noRd
#' @keywords Internal
extract_fields <- function(.query){
  .query |>
    purrr::pluck("url") |>
    httr2::url_parse() |>
    purrr::pluck("query", "fields") |>
    strsplit(split = ",") |>
    purrr::pluck(!!!list(1))
}
