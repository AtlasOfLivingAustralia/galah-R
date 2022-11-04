#' Group by for object of class `data_request`
#' @description `r lifecycle::badge("experimental")` 
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @exportS3Method dplyr::group_by
#' @export
group_by.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  df <- parse_group_by(dots)
  attr(df, "expand") <- TRUE # differs from `galah_group_by`
  update_galah_call(.data, group_by = df)
}