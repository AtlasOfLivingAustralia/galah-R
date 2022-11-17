#' Specify fields to group when downloading record counts
#'
#' `atlas_counts` supports server-side grouping of data. Grouping can be 
#' used to return record counts grouped by multiple, valid fields (found by 
#' `search_all(fields)`. Use `galah_group_by` when using the 
#' `group_by` argument of `atlas_counts` to return record counts summed
#' by one or more valid fields.
#' `r lifecycle::badge("experimental")`
#' @param .data An object of class `data_request`
#' @param ... zero or more individual column names to include
#' @param expand `logical`: When passed to `group_by` argument of 
#' `atlas_counts`, should factor levels be expanded? Defaults to `TRUE`.
#' @return If any arguments are provided, returns a `data.frame` with
#' columns `name` and `type`, as per [galah_select()]; if no arguments
#' are provided, returns `NULL`.
#' @importFrom dplyr group_by
#' @export
group_by.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  df <- parse_group_by(dots)
  attr(df, "expand") <- TRUE # differs from `galah_group_by`
  update_galah_call(.data, group_by = df)
}