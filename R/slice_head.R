#' Slice for object of class `data_request`
#' 
#' @description `r lifecycle::badge("experimental")` 
#' This is a simple function to set the 'limit' argument in [atlas_counts()]
#' using `dplyr` syntax.
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param ... currently ignored
#' @param n The number of rows to be returned. If data are grouped 
#' (using [group_by]), this operation will be performed on each group.
#' @param prop currently ignored, but could be added later
#' @param by currently ignored
#' @importFrom tibble tibble
#' @rdname slice_head
#' @export
slice_head.data_request <- function(.data, ..., n, prop){
  if(!missing(n)){.data$slice <- tibble(n = n)}
  else if(!missing(prop)){.data$slice <- tibble(prop = prop)}
  return(.data)
}

#' @rdname slice_head
#' @export
slice_head.metadata_request <- slice_head.data_request
