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
#' queries; and `query` or `query_set` once constructed (via [as_query()] or
#' [coalesce()]).
#' @param ... Arguments passed on to other methods
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
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
#'  - Any other information retained from the preceeding `_request` object (see [galah_call()])
#'  
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [as_query()], [coalesce()], 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @export
collapse.data_request <- function(x, ..., mint_doi){
  coalesce(x, mint_doi, ...) |>
    collapse()
}

# if calling `collapse()` after `request_metadata()`
#' @rdname collapse.data_request
#' @order 2
#' @export
collapse.metadata_request <- function(x, ...){
  coalesce(x, ...) |>
    collapse()
}

# if calling `collapse()` after `request_files()`
#' @rdname collapse.data_request
#' @order 3
#' @param thumbnail Logical: should thumbnail-size images be returned? Defaults 
#' to `FALSE`, indicating full-size images are required.
#' @importFrom purrr pluck
#' @export
collapse.files_request <- function(x,
                                   # prefix? could be useful for file names
                                   thumbnail = FALSE,
                                   ...
                                   ){
  # convert to `query_set` then parse
  coalesce(x, 
           thumbnail = thumbnail, 
           ...) |>
    collapse()
}

# if calling `collapse()` after `coalesce()`
#' @rdname collapse.data_request
#' @order 4
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
      collapse_query()
  }
}

# if calling `collapse()` after `as_query()`
#' @rdname collapse.data_request
#' @order 4
#' @export
collapse.query <- function(x, ...){
  x
}