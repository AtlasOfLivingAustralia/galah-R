#' Specify fields to group when downloading record counts
#'
#' `ala_counts` supports server-side grouping of data. Grouping can be 
#' used to return record counts grouped by multiple, valid fields (found by 
#' `search_fields`. Use `galah_group_by` when using the 
#' `group_by` argument of `ala_counts` to return record counts summed
#' by one or more valid fields.
#' @param ... zero or more individual column names to include
#' @param expand `logical`: When passed to `group_by` argument of 
#' `ala_counts`, should factor levels be expanded? Defaults to `FALSE`.
#' @seealso [galah_select()], [galah_filter()] and
#' [galah_location()] for related methods.
#' @examples
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
#' 
#'     
#' @export

galah_group_by <- function(..., expand = FALSE){
  df <- galah_select(...)
  class(df) <- c("data.frame", "galah_group_by") 
  attr(df, "expand") <- expand
  df
}
