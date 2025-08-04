#' @title Generate a query
#' @description `collapse()` constructs a valid query so it can be 
#' inspected before being sent. It typically occurs at the end of a pipe,
#' traditionally begun with `galah_call()`, that is used to define a query.
#' As of version 2.0, objects of class `data_request` (created using 
#' `request_data()`), `metadata_request` (from `request_metadata()`) or 
#' `files_request` (from `request_files()`) are all supported by `collapse()`. 
#' Any of these objects can be created using `galah_call()` via the `method`
#' argument.
#' @name collapse.data_request
#' @order 1
#' @param x An object to run `collapse()` on. Classes supported by `galah` 
#' include `data_request`, `metadata_request` and `files_request` for building
#' queries; and `query` or `query_set` once constructed (via [as_query()] or
#' [coalesce()]).
#' @param ... Arguments passed on to other methods
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @return An object of class `query`, which is a list-like object containing at 
#' least the slots `type` and `url`.
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