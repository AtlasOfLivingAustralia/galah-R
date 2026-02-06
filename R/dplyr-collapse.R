#' Generate a query
#' 
#' This function constructs a query so it can be inspected before being sent. It 
#' is typically called at the end of a pipe begun with [galah_call()]. Objects 
#' of class `data_request` (created using [request_data()]), `metadata_request` 
#' (from [request_metadata()]) or `files_request` (from [request_files()]) are 
#' all supported. Any of these objects can be created using [galah_call()] via 
#' the `method` argument.
#' @name collapse.data_request
#' @order 1
#' @param x An object to run `collapse()` on. Classes supported by `galah` 
#' include `data_request`, `metadata_request` and `files_request` for building
#' queries; and `prequery`, `query` or `query_set` once constructed (via 
#' [capture()] or [compound()]).
#' @param ... Arguments passed on to [capture()].
#' @return An object of class `query`, which is a list-like object containing 
#' two or more of the following slots:
#' 
#'  - `type`: The type of query, serves as a lookup to the corresponding field in `show_all(apis)`
#'  - `url`: Either:
#'    - a length-1 character giving the API to be queried; or 
#'    - a `tibble` containing at least the field `url` and optionally others
#'  - `headers`: headers to be sent with the API call
#'  - `body`: body section of the API call
#'  - `options`: options section of the API call
#'  - Any other information retained from the preceeding `_request` object (see [capture()])
#'  
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [capture()], [compound()], 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @export
collapse.data_request <- function(x, ...){
  x |>
    compound(...) |>
    collapse()
}

#' @rdname collapse.data_request
#' @order 2
#' @export
collapse.metadata_request <- collapse.data_request

#' @rdname collapse.data_request
#' @order 3
#' @export
collapse.files_request <- collapse.data_request

#' @rdname collapse.data_request
#' @order 4
#' @export
collapse.prequery <- collapse.data_request

#' @rdname collapse.data_request
#' @order 5
#' @export
collapse.query <- function(x, ...){
  x
}

# if calling `collapse()` after `compound()`
#' @rdname collapse.data_request
#' @order 6
#' @export
collapse.query_set <- function(x, ...){
  # note: files requests do not need to call build_checks()
  if(grepl("^files", x[[1]]$type)){
    x |>
      purrr::pluck(!!!list(1))
  }else{
    x |>
      collapse_build_checks() |>
      collapse_run_checks() |>
      collapse_query_set()
  }
}