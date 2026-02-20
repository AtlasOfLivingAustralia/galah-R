#' @noRd
#' @keywords Internal
collect_events <- function(.query, wait = TRUE, file = NULL){
  browser()
}

#' @noRd
#' @keywords Internal
collect_events_count <- function(.query){

  # get response from API
  result <- query_API(.query)

  tibble::tibble(count = unlist(result))
  # purrr::pluck(result, "data", "eventSearch", "documents", "total")
}