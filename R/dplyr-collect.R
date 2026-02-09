#' Retrieve a database query
#' 
#' @description
#' Retrieve the result of a query from the server. It is the 
#' default way to end a piped query that begins with [galah_call()].
#' @name collect.data_request
#' @order 1
#' @param x An object of class `data_request`, `metadata_request` or 
#' `files_request` (from `galah_call()`); or an object of class `prequery`,
#' `query_set` or `query` (from [capture()],
#' \code{\link[=collapse.data_request]{collapse()}}
#' or \code{\link[=compute.data_request]{compute()}})
#' @param ... Arguments passed on to other methods
#' @param wait logical; should `galah` wait for a response? Defaults to `FALSE`.
#' Only applies for `type = "occurrences"` or `"species"`.
#' @param file (Optional) file name. If not given, will be set to `data` with 
#' date and time added. The file path (directory) is always given by 
#' `galah_config()$package$directory`.
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
#' \code{\link[=collect.data_request]{collect()}} is the final step of the 
#' [galah_call()] workflow, and it retrieves the result of a 
#' query once it is processed by the server.
#' @return In most cases, `collect()` returns a `tibble` containing requested 
#' data. Where the requested data are not yet ready (i.e. for occurrences when
#' `wait` is set to `FALSE`), this function returns an object of class `query`
#' that can be used to recheck the download at a later time.
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [capture()], [compound()], 
#' \code{\link[=collapse.data_request]{collapse()}} or 
#' \code{\link[=compute.data_request]{compute()}}.
#' @export
collect.data_request <- function(x, ..., wait = TRUE, file = NULL){
  collapse(x, ...) |>
    compute() |>
    collect(wait = wait, file = file)
}

#' @rdname collect.data_request
#' @order 2
#' @export
collect.metadata_request <- function(x, ...){
  collapse(x, ...) |>
    compute() |>
    collect()
}

#' @rdname collect.data_request
#' @order 3
#' @export
collect.files_request <- function(x, ...){
  collapse(x, ...) |> 
    compute() |>
    collect()
}

#' @rdname collect.data_request
#' @order 4
#' @export
collect.prequery <- function(x, ...){
  compute(x) |>
    collect(...)
}

#' @rdname collect.data_request
#' @order 5
#' @export
collect.query <- function(x, ...){
  compute(x) |>
    collect(...)
}

#' @rdname collect.data_request
#' @order 6
#' @export
collect.query_set <- function(x,
                              ...,
                              wait = TRUE, 
                              file = NULL
                              ){
  x |>
    collapse(...) |>
    compute() |>
    collect(wait = wait, file = file)
}

#' @rdname collect.data_request
#' @order 7
#' @export
collect.computed_query <- function(x, 
                          ...,
                          wait = TRUE, 
                          file = NULL
                          ){
  # sometimes no url is given, e.g. when a search returns no data
  if(is.null(x$url) & # most queries have a `url`
     is.null(x$data) & # some cached metadata queries have `data` instead
     is.null(x$status) # finally, after `compute()`, occurrences have `status`
     ){
    tibble::tibble()
  }else{
    switch(x$type,
           "data/distributions" = collect_distributions(x),
           "data/occurrences" = collect_occurrences(x, wait = wait, file = file),
           "data/occurrences-count" = collect_occurrences_count(x),
           "data/occurrences-count-groupby" = collect_occurrences_count(x),
           "data/occurrences-doi" = collect_occurrences_doi(x, file = file),
           "data/occurrences-glimpse" = collect_occurrences_glimpse(x),
           "data/species"= collect_species(x, file = file),
           "data/species-count" = collect_species_count(x),
           # "data/taxonomy" = collect_taxonomy(x),
           "files/media" = collect_media_files(x),
           "metadata/apis" = collect_apis(x),
           "metadata/assertions" = collect_assertions(x),
           "metadata/atlases" = collect_atlases(x),
           "metadata/collections" = collect_collections(x),
           "metadata/config" = collect_config(x),
           "metadata/datasets" = collect_datasets(x),
           "metadata/distributions" = collect_distributions_metadata(x),
           "metadata/fields" = collect_fields(x),
           "metadata/fields-unnest" = collect_fields_unnest(x),
           "metadata/licences" = collect_licences(x),
           "metadata/lists" = collect_lists(x),
           "metadata/lists-unnest" = collect_lists_unnest(x),
           "metadata/media" = collect_media_metadata(x),
           "metadata/profiles" = collect_profiles(x),
           "metadata/profiles-unnest" = collect_profiles_unnest(x),
           "metadata/providers" = collect_providers(x),
           "metadata/ranks" = collect_ranks(x),
           "metadata/reasons" = collect_reasons(x),
           "metadata/taxa-single" = collect_taxa(x),
           "metadata/taxa-multiple" = collect_taxa(x),
           "metadata/taxa-unnest" = collect_taxa_unnest(x),
           "metadata/identifiers" = collect_identifiers(x),
           cli::cli_abort("Unrecognised `type`"))
  }
}
