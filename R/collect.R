#' @title Retrieve a database query
#'
#' @description `collect()` attempts to retrieve the result of a query from the 
#' selected API. 
#' @name collect_galah
#' @order 1
#' @param .data An object of class `data_request`, `metadata_request` or 
#' `files_request` (from `galah_call()`); or an oject of class `query_set` or 
#' `query` (from `collapse()` or `compute()`)
#' @param wait logical; should `galah` wait for a response? Defaults to FALSE.
#' Only applies for `type = "occurrences"` or `"species"`.
#' @param file (optional) file name. If not given will be `data` with date and 
#' time added. File path is always given by `galah_config()$package$directory`.
#' @return In most cases, `collect()` returns a `tibble` containing requested 
#' data. Where the requested data are not yet ready (i.e. for occurrences when
#' `wait` is set to `FALSE`), this function returns an object of class `query`
#' that can be used to recheck the download at a later time.
#' @export
collect.data_request <- function(.data, wait = TRUE, file = NULL){
  collapse(.data) |>
    compute() |>
    collect(wait = wait, file = file)
}

#' @rdname collect_galah
#' @order 2
#' @export
collect.metadata_request <- function(.data){
  collapse(.data) |>
    compute() |>
    collect()
}

#' @rdname collect_galah
#' @order 3
#' @export
collect.files_request <- function(.data){
  collapse(.data) |> 
    compute() |>
    collect()
}

#' @rdname collect_galah
#' @order 4
#' @export
collect.query_set <- function(.data, wait = TRUE, file = NULL){
  compute(.data) |>
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
collect.query <- function(.data, 
                          wait = TRUE, 
                          file = NULL # FIXME: is `file` used?
                          ){
  # sometimes no url is given, e.g. when a search returns no data
  if(is.null(.data$url) & # most queries have a `url`
     is.null(.data$data) & # some cached metadata queries have `data` instead
     is.null(.data$status) # finally, after `compute()`, occurrences have `status`
     ){
    tibble()
  }else{
    switch(.data$type,
           "data/occurrences" = collect_occurrences(.data, wait = wait, file = file),
           "data/occurrences-count" = collect_occurrences_count(.data),
           "data/occurrences-count-groupby" = collect_occurrences_count(.data),
           "data/occurrences-doi" = collect_occurrences_doi(.data),
           "data/species"= collect_species(.data, file = file),
           "data/species-count" = collect_species_count(.data),
           "data/taxonomy" = collect_taxonomy(.data),
           "files/media" = collect_media_files(.data),
           "metadata/apis" = collect_apis(.data),
           "metadata/assertions" = collect_assertions(.data),
           "metadata/atlases" = collect_atlases(.data),
           "metadata/collections" = collect_collections(.data),
           "metadata/datasets" = collect_datasets(.data),
           "metadata/fields" = collect_fields(.data),
           "metadata/fields-unnest" = collect_fields_unnest(.data),
           "metadata/licences" = collect_licences(.data),
           "metadata/lists" = collect_lists(.data),
           "metadata/lists-unnest" = collect_lists_unnest(.data),
           "metadata/media" = collect_media_metadata(.data),
           "metadata/profiles" = collect_profiles(.data),
           "metadata/profiles-unnest" = collect_profiles_unnest(.data),
           "metadata/providers" = collect_providers(.data),
           "metadata/ranks" = collect_ranks(.data),
           "metadata/reasons" = collect_reasons(.data),
           "metadata/taxa-single" = collect_taxa(.data),
           "metadata/taxa-multiple" = collect_taxa(.data),
           "metadata/taxa-unnest" = collect_taxa_unnest(.data),
           "metadata/identifiers" = collect_identifiers(.data),
           abort("unrecognised `type`"))
  }
}