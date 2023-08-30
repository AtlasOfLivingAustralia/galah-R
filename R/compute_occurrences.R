#' Internal function to `compute()` for `type = "occurrences"`
#' @noRd
#' @keywords Internal
compute_occurrences <- function(.data){
  switch(pour("atlas", "region"),
         "United Kingdom" = compute_occurrences_uk(.data),
         "Global" = compute_occurrences_gbif(.data),
         compute_occurrences_la(.data))
}

#' Internal function to `compute()` for `type = "occurrences"` for UK
#' @noRd
#' @keywords Internal
compute_occurrences_uk <- function(.data){
  check_reason(.data)
  class(.data) <- "data_response"
  return(.data)
}

#' Internal function to `compute()` for `type = "occurrences"` for GBIF
#' Note: this has not been updated and is unlikely to work
#' @noRd
#' @keywords Internal
compute_occurrences_gbif <- function(.data){
  post_result <- query_API(.data)
  status_code <- list(url = paste0(
    "https://api.gbif.org/v1/occurrence/download/", 
    post_result)) |>
    query_API()
  result <- c(
    list(type = "occurrences"),
    status_code)
  class(result) <- "data_response"
  return(result)
}

#' Internal function to `compute()` for `type = "occurrences"` for ALA
#' @noRd
#' @keywords Internal
compute_occurrences_la <- function(.data){
  check_reason(.data)
  status_code <- query_API(.data) |>
    as.list() |>
    check_occurrence_response()
  if(pour("package", "verbose")){
    n_records <- status_code$total_records
    inform(glue("Request for {n_records} occurrences placed in queue"))
  }
  result <- c(
    list(type = "occurrences"),
    status_code)
  class(result) <- "data_response"
  return(result)
}

#' Internal function to check that a reason code is valid
#' @noRd
#' @keywords Internal
check_reason <- function(.data, error_call = caller_env()){
  query <- url_parse(.data$url)$query
  if(is.null(query$reasonTypeId)){
    bullets <- c("Please supply a valid download reason",
                 i = "Use `show_all(reasons)` to see all valid reasons.",
                 i = "Use `galah_config(download_reason_id = ...) to set a reason.")
    abort(bullets, call = error_call) 
  }else{
    value <- as.integer(query$reasonTypeId)
    if(!(value %in% show_all_reasons()$id)){
      bullets <- c(
        "Invalid download reason ID.",
        i = "Use `show_all(reasons)` to see all valid reasons.",
        x = glue("{value} does not match an existing reason ID."))
      abort(bullets, call = error_call)    
    }
  }
}