#' Narrow a query by specifying filters
#'
#' "Filters" are arguments of the form `field` `logical` `value` that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (`year == 2020`), or to return all records except for fossils
#'  (`basisOfRecord != "FossilSpecimen"`).
#'  
#' `r lifecycle::badge("experimental")` 
#' @seealso  [galah_filter()], with which this function is synonymous.
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param ... filters, in the form `field logical value`
#' @export
filter.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
  update_galah_call(.data, filter = parse_filter(dots))
}