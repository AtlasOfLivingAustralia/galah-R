#' @title Retrieve a database query
#'
#' @description `collect()` attempts to retrieve the result of a query from the 
#' selected API. 
#' @name collect_galah
#' @order 1
#' @param x An object of class `data_request`, `metadata_request` or 
#' `files_request` (from `galah_call()`); or an oject of class `query_set` or 
#' `query` (from `collapse()` or `compute()`)
#' @param ... Arguments passed on to other methods
#' @param wait logical; should `galah` wait for a response? Defaults to FALSE.
#' Only applies for `type = "occurrences"` or `"species"`.
#' @param file (Optional) file name. If not given, will be set to `data` with 
#' date and time added. The file path (directory) is always given by 
#' `galah_config()$package$directory`.
#' @return In most cases, `collect()` returns a `tibble` containing requested 
#' data. Where the requested data are not yet ready (i.e. for occurrences when
#' `wait` is set to `FALSE`), this function returns an object of class `query`
#' that can be used to recheck the download at a later time.
#' @export
collect.data_request <- function(x, ..., wait = TRUE, file = NULL){
  collapse(x, ...) |>
    compute() |>
    collect(wait = wait, file = file)
}

#' @rdname collect_galah
#' @order 2
#' @export
collect.metadata_request <- function(x, ...){
  collapse(x, ...) |>
    compute() |>
    collect()
}

#' @rdname collect_galah
#' @order 3
#' @export
collect.files_request <- function(x, ...){
  collapse(x, ...) |> 
    compute() |>
    collect()
}

#' @rdname collect_galah
#' @order 4
#' @export
collect.query <- function(x, ..., wait = TRUE, file = NULL){
  compute(x) |>
    collect(wait = wait, file = file)
}

#' @rdname collect_galah
#' @order 5
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
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
    tibble()
  }else{
    switch(x$type,
           "data/occurrences" = collect_occurrences(x, wait = wait, file = file),
           "data/occurrences-count" = collect_occurrences_count(x),
           "data/occurrences-count-groupby" = collect_occurrences_count(x),
           "data/occurrences-doi" = collect_occurrences_doi(x, file = file),
           "data/species"= collect_species(x, file = file),
           "data/species-count" = collect_species_count(x),
           # "data/taxonomy" = collect_taxonomy(x),
           "files/media" = collect_media_files(x),
           "metadata/apis" = collect_apis(x),
           "metadata/assertions" = collect_assertions(x),
           "metadata/atlases" = collect_atlases(x),
           "metadata/collections" = collect_collections(x),
           "metadata/datasets" = collect_datasets(x),
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
           abort("unrecognised `type`"))
  }
}
