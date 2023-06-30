#' Slice for object of class `data_request`
#' 
#' This is a simple function to set the 'limit' argument in [atlas_counts()]
#' using `dplyr` syntax.
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param ... currently ignored
#' @param n The number of rows to be returned. If data are grouped 
#' (using [group_by]), this operation will be performed on each group.
#' @param prop currently ignored
#' @returns An updated object of class `data_request`, whose `limit` argument
#' is set with an integer value provided by `n`.
#' @export
slice_head.data_request <- function(.data, ..., n, prop){
  update_galah_call(.data, limit = n)
}