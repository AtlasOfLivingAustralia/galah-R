#' Slice for object of class `data_request`
#' 
#' @description `r lifecycle::badge("experimental")` 
#' This is a simple function to set the 'limit' argument in [atlas_counts()]
#' using `dplyr` syntax.
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param n The number of rows to be returned. If data are grouped 
#' (using [group_by]), this operation will be performed on each group.
#' @exportS3Method dplyr::slice_head
#' @export
slice_head.data_request <- function(.data, n){
  update_galah_call(.data, limit = n)
}