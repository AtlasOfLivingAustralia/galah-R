#' Specify fields to group when downloading record counts
#'
#' \code{ala_counts} supports server-side grouping of data. Grouping can be 
#' used to return record counts grouped by multiple, valid fields (found by 
#' \code{search_fields}. Use \code{galah_group_by} when using the 
#' \code{group_by} argument of \code{ala_counts} to return record counts summed
#' by one or more valid fields.
#' @param ... zero or more individual column names to include
#' @param expand \code{logical}: When passed to \code{group_by} argument of 
#' \code{ala_counts}, should factor levels be expanded? Defaults to \code{FALSE}.
#' @seealso \code{\link{galah_select}}, \code{\link{galah_filter}} and
#' \code{\link{galah_location}} for related methods.
#' @examples \dontrun{
#' # Return record counts since 2010 by year
#' ala_counts(
#'     filter = galah_filter(year > 2010),
#'     group_by = galah_group_by(year)
#'     )
#'     
#' # Return record counts since 2010 by year and data provider
#' ala_counts(
#'     filter = galah_filter(year > 2010),
#'     group_by = galah_group_by(year, dataResourceName, expand = TRUE)
#'     )
#'     
#' # Return record counts of Litoria species each year since 2015, limiting
#' # results to the top 5 each year.
#' ala_counts(
#'     taxa = select_taxa("Litoria"),
#'     filter = galah_filter(year > 2015),
#'     group_by = galah_group_by(year, species, expand = TRUE),
#'     limit = 5)
#' @export

galah_group_by <- function(..., expand = FALSE){
  df <- galah_select(...)
  class(df) <- c("data.frame", "galah_group_by") 
  attr(df, "expand") <- expand
  df
}