#' Get a glimpse of your data
#' 
#' [glimpse()] is like a transposed version of [print()]: columns run down the page, 
#' and data runs across. This makes it possible to see every column in a data 
#' frame. It's a little like [str()] applied to a data frame but it tries to 
#' show you as much data as possible. This implementation is specific 
#' to `galah` and is evaluated lazily. `r lifecycle::badge("experimental")`
#' @param x An object of class `data_request`
#' @param ... Other arguments, currently ignored
#' @details
#' This implementation of [glimpse()] actually involves changing the API call
#' sent to the server, then returning a novel object class with it's own 
#' [print()] method.
#' @name glimpse.data_request
#' @examples \dontrun{
#' galah_call() |>
#'   filter(year >= 2019,
#'          basisOfRecord == "HumanObservation") |>
#'   select(year, basisOfRecord, species) |>
#'   glimpse() |>
#'   collect()
#' }
#' @export
glimpse.data_request <- function(x, ...){
  update_request_object(x, glimpse = TRUE)
}

#' @rdname glimpse.data_request
#' @export
print.occurrences_glimpse <- function(x, ...){
  y <- utils::capture.output(dplyr::glimpse(x))
  n_text <- attr(x, 'total_n') |>
    formatC(big.mark = ",")
  y[[1]] <- glue::glue("Rows: {n_text}")
  cli::cat_line(y)
}