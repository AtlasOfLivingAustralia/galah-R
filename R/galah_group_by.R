#' Specify fields to group when downloading record counts
#'
#' `atlas_counts` supports server-side grouping of data. Grouping can be 
#' used to return record counts grouped by multiple, valid fields (found by 
#' `search_fields`. Use `galah_group_by` when using the 
#' `group_by` argument of `atlas_counts` to return record counts summed
#' by one or more valid fields.
#' @param ... zero or more individual column names to include
#' @param expand `logical`: When passed to `group_by` argument of 
#' `atlas_counts`, should factor levels be expanded? Defaults to `TRUE`.
#' @return If any arguments are provided, returns a `data.frame` with
#' columns `name` and `type`, as per [galah_select()]; if no arguments
#' are provided, returns `NULL`.
#' @seealso [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for related methods.
#' @examples
#' # Return record counts since 2010 by year
#' atlas_counts(
#'     filter = galah_filter(year > 2010),
#'     group_by = galah_group_by(year)
#'     )
#'     
#' # Return record counts since 2010 by year and data provider
#' atlas_counts(
#'     filter = galah_filter(year > 2010),
#'     group_by = galah_group_by(year, dataResourceName)
#'     )
#'     
#' # Return record counts of Litoria species each year since 2015, limiting
#' # results to the top 5 each year.
#' atlas_counts(
#'     taxa = search_taxa("Litoria"),
#'     filter = galah_filter(year > 2015),
#'     group_by = galah_group_by(year, species),
#'     limit = 5)
#' 
#'     
#' @export

galah_group_by <- function(..., expand = TRUE){
  if(length(as.list(match.call(expand.dots = FALSE)$...)) > 0){
    df <- galah_select(...)
    class(df) <- append(class(df), "galah_group_by")
    attr(df, "expand") <- expand
    df
  }else{NULL}
}