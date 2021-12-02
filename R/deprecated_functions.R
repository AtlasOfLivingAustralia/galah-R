#' Specify fields for occurrence download
#' 
#' @param ... zero or more individual column names to include
#' @param group \code{string}: (optional) name of one or more column groups to
#' include. Valid options are \code{"basic"}, \code{"event"} and
#' \code{"assertion"}
#' @describeIn galah_select Deprecated function name
#' @export
select_columns <- function(..., group){
  galah_select(..., group)
}

#' Choose filters to narrow down occurrence queries
#'
#' @param ... filters, in the form \code{field logical value}
#' @param profile \code{string}: (optional) a data quality profile to apply to the
#' records. See \code{\link{find_profiles}} for valid profiles. By default
#' no profile is applied.
#' @describeIn galah_filter Deprecated function name
#' @export
select_filters <- function(..., profile = NULL) {
  galah_filter(..., profile)
}


#' Build a WKT string from an sf spatial object or verify an existing WKT
#'
#' @param query wkt string or sf object
#' @describeIn galah_locations Deprecated function name
#' @export
select_locations <- function(query) {
  galah_locations(query)
}
