#' Narrow a query by specifying filters
#'
#' "Filters" are arguments of the form `field` `logical` `value` that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (`year == 2020`), or to return all records except for fossils
#'  (`basisOfRecord != "FossilSpecimen"`). Note that there is a performance
#' benefit to using `filter` rather than `galah_filter`, albeit a minor one.
#'  
#' `r lifecycle::badge("experimental")` 
#' @seealso  [galah_filter()], with which this function is (nearly) synonymous.
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param ... filters, in the form `field logical value`
#' @export
filter.data_request <- function(.data, ..., new_code = TRUE){
  dots <- enquos(..., .ignore_empty = "all")
  if(new_code){
    update_galah_call(.data, filter = parse_quosures(dots))
  }else{
    check_filter(dots)
    update_galah_call(.data, filter = parse_filter(dots))  
  }
}