#' Internal function for calling `collect()` on type = "events"
#' @noRd
#' @keywords Internal
collect_events <- function(.query,
                           wait = TRUE,
                           file = NULL,
                           call = rlang::caller_env){
  browser()
}

#' Internal function for calling `collect()` on type = "events-count"
#' @noRd
#' @keywords Internal
collect_events_count <- function(.query, call = rlang::caller_env()){

  # get response from API
  result <- query_API(.query, error_call = call)

  tibble::tibble(count = unlist(result))
  # purrr::pluck(result, "data", "eventSearch", "documents", "total")
}

#' Internal function for calling `collect()` on type = "events-describe"
#' @noRd
#' @keywords Internal
collect_events_describe <- function(.query, call = rlang::caller_env()){

  # get response from API
  result <- query_API(.query, error_call = call)

  # parse to a format consistent with `collect_occurrences_describe()`
  result |>
    purrr::pluck("data", "__schema", "types") |>
    dplyr::bind_rows() |>
    dplyr::rename("id" = .data$name,
                  "data_type" = .data$kind) |>
    dplyr::relocate(.data$description, .before = .data$data_type)
}
