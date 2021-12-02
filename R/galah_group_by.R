#' Specify fields for occurrence download
#'
#' \code{ala_counts} supports server-side grouping of data, which relies on this
#' function.
#' @param ... zero or more individual column names to include
#' @param expand \code{logical}: When passed to \code{group_by} argument of 
#' \code{ala_counts}, should factor levels be expanded? Defaults to \code{FALSE}.
#' @seealso \code{\link{galah_select}}, \code{\link{galah_filter}} and
#' \code{\link{galah_location}} for related methods.
#' @export

galah_group_by <- function(..., expand = FALSE){
  df <- galah_select(...)
  class(df) <- c("data.frame", "galah_group_by") 
  attr(df, "expand") <- expand
  df
}