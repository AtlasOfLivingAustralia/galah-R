#' @noRd
#' @keywords Internal
collapse_events <- function(x){
  browser()
}

#' @noRd
#' @keywords Internal
collapse_events_count <- function(x){

    result <- list(eventSearch = list(documents = list(total = list()))) |>
      jsonlite::toJSON(auto_unbox = TRUE,
                       pretty = FALSE)

    x$body <- glue::glue("query list {result}") |>
      stringr::str_remove_all(":\\[\\]") |>
      stringr::str_remove_all(":|\"") 

  x
}