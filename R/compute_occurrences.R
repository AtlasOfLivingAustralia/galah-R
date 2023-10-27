#' Internal function to `compute()` for `type = "occurrences"`
#' @noRd
#' @keywords Internal
compute_occurrences <- function(q_obj){
  switch(pour("atlas", "region"),
         "United Kingdom" = compute_occurrences_uk(q_obj),
         "Global" = compute_occurrences_gbif(q_obj),
         compute_occurrences_la(q_obj))
}

#' Internal function to `compute()` for `type = "occurrences"` for UK
#' @noRd
#' @keywords Internal
compute_occurrences_uk <- function(q_obj){
  q_obj
}

#' Internal function to `compute()` for `type = "occurrences"` for GBIF
#' Note: this has not been updated and is unlikely to work
#' @noRd
#' @keywords Internal
compute_occurrences_gbif <- function(q_obj){
  post_result <- query_API(q_obj) # returns an id
  status_code <- list(
    type = "data/occurrences",
    url = paste0("https://api.gbif.org/v1/occurrence/download/", 
                 post_result)) |>
    query_API() |>
    check_occurrence_response()
  result <- c(
    list(type = "data/occurrences"),
    status_code)
  class(result) <- "query"
  return(result)
}

#' Internal function to `compute()` for `type = "occurrences"` for ALA
#' @noRd
#' @keywords Internal
compute_occurrences_la <- function(q_obj){
  status_code <- query_API(q_obj) |>
    as.list() |>
    check_occurrence_response()
  if(pour("package", "verbose")){
    n_records <- status_code$total_records
    inform(glue("Request for {n_records} occurrences placed in queue"))
  }
  result <- c(
    list(type = "data/occurrences"),
    status_code)
  class(result) <- "query"
  result
}
