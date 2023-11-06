#' Subset first rows of `data_request`
#' 
#' @description 
#' `r lifecycle::badge("experimental")`  
#' 
#' This is a simple function to set the `limit` argument in [atlas_counts()]
#' using `dplyr` syntax. As of galah 2.0.0, `slice_head()` is only supported in 
#' queries of type `occurrences-count()`, or metadata requests. Note also that 
#' `slice_head()` is lazily evaluated; it only affects a query once it is run by
#' `compute()` or (more likely) `collect()`.
#' 
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param ... currently ignored
#' @param n The number of rows to be returned. If data are grouped 
#' (using [group_by]), this operation will be performed on each group.
#' @param prop currently ignored, but could be added later
#' @param by currently ignored
#' @examples \dontrun{
#' # Limit number of rows returned to 3.
#' # In this case, our query returns the top 3 years with most records.
#' galah_call() |>
#'   identify("perameles") |>
#'   filter(year > 2010) |>
#'   group_by(year) |>
#'   count() |>
#'   slice_head(n = 3) |>
#'   collect()
#' }
#' @importFrom tibble tibble
#' @rdname slice_head
#' @export
slice_head.data_request <- function(.data, ..., n, prop, by = NULL){
  # handle inputs
  if(!missing(n)){
     result <- tibble(slice_n = n)
  }else if(!missing(prop)){
    result <- tibble(slice_prop = prop)
  } else {
    result <- tibble()
  }
  
  # if no data - or NULL - is provided, make no updates
  if(length(result) < 1){
    .data
  }else{
    result$slice_called <- TRUE # distinguish between user calls to `slice()`
    # as opposed to auto-setting of slice
    .data$slice <- result
    return(.data)    
  }
}

#' @rdname slice_head
#' @export
slice_head.metadata_request <- slice_head.data_request

# TODO:
# support `slice_tail()`
# support `slice_` with `prop` (as well as `n`)