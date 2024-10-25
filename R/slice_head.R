#' Subset rows using their positions
#' 
#' @description 
#' `r lifecycle::badge("experimental")`  
#' 
#' `slice()` lets you index rows by their (integer) locations. For objects of
#' classes `data_request` or `metadata_request`, only `slice_head()` is
#' currently implemented, and selects the first `n` rows.
#' 
#' If `.data` has been grouped using 
#' \code{\link[=group_by.data_request]{group_by()}}, the operation will be 
#' performed on each group, so that (e.g.) `slice_head(df, n = 5)` will select 
#' the first five rows in each group.
#' @name slice_head.data_request
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param ... Currently ignored
#' @param n The number of rows to be returned. If data are grouped 
#' \code{\link[=group_by.data_request]{group_by()}}, this operation will be 
#' performed on each group.
#' @param prop Currently ignored.
#' @param by Currently ignored.
#' @returns An amended `data_request` with a completed `slice` slot.
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

#' @rdname slice_head.data_request
#' @export
slice_head.metadata_request <- slice_head.data_request

# TODO:
# support `slice_tail()`
# support `slice_` with `prop` (as well as `n`)