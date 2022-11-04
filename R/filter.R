#' Filter for object of class `data_request`
#' @description `r lifecycle::badge("experimental")` 
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @exportS3Method dplyr::filter
#' @export
filter.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
  update_galah_call(.data, filter = parse_filter(dots))
}