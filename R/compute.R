#' @title Compute a query
#' @description `compute()` is useful for several purposes. It's original 
#' purpose is to send a request for data, which can then be processed by the 
#' server and retrieved at a later time (via `collect()`).
#' @name compute.data_request
#' @order 1
#' @param x An object of class `data_request`, `metadata_request` or 
#' `files_request` (i.e. constructed using a pipe) or `query` 
#' (i.e. constructed by `collapse()`) 
#' @param ... Arguments passed on to other methods
#' @return An object of class `computed_query`, which is identical to class
#' `query` except for occurrence data, where it also contains information on the 
#' status of the request.
#' @export
compute.data_request <- function(x, ...){
  # x$type <- check_type(x$type) # possibly still needed; unclear
  collapse(x, ...) |>  # converts to an object of class `query`
    compute()
}

# if calling `compute()` after `request_metadata()` 
#' @rdname compute.data_request
#' @order 2
#' @export
compute.metadata_request <- compute.data_request


# if calling `compute()` after `request_files()` 
#' @rdname compute.data_request
#' @order 3
#' @export
compute.files_request <- compute.data_request

# if calling `compute()` on an object extracted from `collapse()` 
#' @rdname compute.data_request
#' @order 5
#' @export
compute.query <- function(x, ...){
  switch(x$type, 
         "data/occurrences" = compute_occurrences(x),
         "data/species" = {
           if(is_gbif()){
             compute_occurrences(x)
           }else{
             as_computed_query(x)
           }
         },
         as_computed_query(x)
  )
}

#' Internal function to convert class `query` to `computed_query`
#' @noRd
#' @keywords Internal
as_computed_query <- function(x){
  class(x) <- "computed_query"
  x
}
