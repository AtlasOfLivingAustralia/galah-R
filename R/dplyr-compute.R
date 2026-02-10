#' Compute a query
#' 
#' @description
#' Sends a request for information to a server. This is useful 
#' for requests that run a server-side process, as it separates the 
#' submission of the request from its retrieval. 
#' 
#' Within galah, `compute()` is generally hidden as it is one part of the overall 
#' process to complete a `data_request`,  
#' `metadata_request` or `file_request`. However, calling 
#' \code{\link[=compute.data_request]{compute()}} at the 
#' end of a [galah_call()] sends a request to be completed server-side 
#' (i.e., outside of R), and the result can be returned in R by 
#' calling \code{\link[=collect.data_request]{collect()}}
#' at a later time. This can be preferable to calling [atlas_occurrences()], which 
#' prevents execution of new code until the server-side process is complete.
#' @name compute.data_request
#' @order 1
#' @param x An object of class `data_request`, `metadata_request` or 
#' `files_request` (i.e. constructed using a pipe) or `query` 
#' (i.e. constructed by \code{\link[=collapse.data_request]{collapse()}}) 
#' @param ... Arguments passed on to other methods
#' @details
#' `galah` uses an object-based pipeline to convert piped requests into
#' valid queries, and to enact those queries with the specified organisation.
#' Typically, requests open with [galah_call()] - though [request_metadata()] 
#' and [request_files()] are also valid - and end with 
#' \code{\link[=collect.data_request]{collect()}}. Under the hood,
#' the sequence of functions is as follows:
#' 
#' [capture()] → [compound()] → 
#' \code{\link[=collapse.data_request]{collapse()}} → 
#' \code{\link[=compute.data_request]{compute()}} → 
#' \code{\link[=collect.data_request]{collect()}}
#' 
#' \code{\link[=compute.data_request]{compute()}} sends a query to a server, 
#' which, once completed, can be retrieved using 
#' \code{\link[=collect.data_request]{collect()}}.
#' @return An object of class `computed_query`, which is identical to class
#' `query` except for occurrence data, where it also contains information on the 
#' status of the request.
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [capture()], [compound()], 
#' \code{\link[=collapse.data_request]{collapse()}}, 
#' \code{\link[=collect.data_request]{collect()}}.
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

# if calling `compute()` after `capture()` 
#' @rdname compute.data_request
#' @order 4
#' @export
compute.prequery <- compute.data_request

# if calling `compute()` on an object extracted from `collapse()` 
#' @rdname compute.data_request
#' @order 5
#' @export
compute.query <- function(x, ...){
  switch(x$type, 
         "data/occurrences" = compute_occurrences(x),
         "data/occurrences-doi" = compute_occurrences_doi(x),
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

#' @rdname compute.data_request
#' @order 6
#' @export
compute.query_set <- function(x, ...){
  x |>
    collapse() |>
    compute()
}

#' Internal function to convert class `query` to `computed_query`
#' @noRd
#' @keywords Internal
as_computed_query <- function(x){
  structure(x, class = "computed_query")
}
