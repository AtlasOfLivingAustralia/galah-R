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
#' @param x An object of class `data_request`, `metadata_request` or 
#' `files_request`
#' @param ... Arguments passed on to other methods
#' @param .expand Logical: should the `query_set` be returned? This object
#' shows all the requisite data needed to process the supplied query. Defaults
#' to `FALSE`; if `TRUE` will append the `query_set` to an extra slot in the
#' `query` object.
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @return An object of class `query`, which is a list-like object containing at 
#' least the slots `type` and `url`.
#' @export
collapse.data_request <- function(x, ..., mint_doi, .expand = FALSE){
  query_set <- build_query_set(x, 
                               mint_doi = mint_doi, 
                               ...)
  result <- query_set |>
    build_checks() |>
    parse_checks() |>
    parse_query()
  if(.expand){
    result$call <- query_set
  }
  result
}

# if calling `collapse()` after `request_metadata()`
#' @rdname collapse.data_request
#' @order 2
#' @export
collapse.metadata_request <- function(x, .expand = FALSE, ...){
  query_set <- build_query_set(x, ...)
  result <- query_set |>
    build_checks() |>
    parse_checks() |>
    parse_query()
  if(.expand){
    result$call <- query_set
  }
  result
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
  build_query_set(x, 
                  thumbnail = thumbnail, 
                  ...) |>
    # note: files requests do not need to call build_checks()
    pluck(!!!list(1))
}
