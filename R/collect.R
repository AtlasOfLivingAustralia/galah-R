#' Force computation of a database query
#'
#' The `collapse()`, `compute()` and `collect()` functions are borrowed from 
#' `dplyr`, and underpin every `atlas_` function in `galah`.
#'  
#' `collapse()` constructs a valid query so it can be 
#' inspected before being sent. `compute()` sends the query so that it can 
#' be calculated server-side in the specified atlas. `collect()` returns the
#' resulting `tibble` once it is complete.
#' 
#' The `collect()` function has three extensions in `galah`:  
#'   *  `collect.data_request()` is designed to be called at the end of a pipe 
#'   *  `collect.data_response()` is called after `compute()`
#'   *  `collect.data_query()` is called after `collapse()`.
#' 
#' @name collect.query
#' @param .data An object of class `data_request`, `data_query` or 
#' `data_response` 
#' @param wait logical; should `galah` wait for a response? Defaults to FALSE.
#' Only applies for `type = "occurrences"` or `"species"`.
#' @param file (optional) file name. If not given will be `data` with date and 
#' time added. File path is always given by `galah_config()$package$directory`.
#' @return `collect()` returns a `tibble` containing requested data. `compute()`
#' returns an object of class `data_response`. `collapse()` returns an object of 
#' class `data_query`.
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
#' @export
collect.query <- function(.data, wait = FALSE, file = NULL){
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

# note: might be sensible to change above to `collect.response()` or similar,
# so as to enforce the collapse() > compute() > collect() sequence.
# BUT we do this in reverse; collapse() always returns a `query_set`
# which only becomes a single `query` via `compute()`

#' @rdname collect.query
#' @export
collect.query_set <- function(.data, wait = FALSE, file = NULL){
  compute(.data) |>
    collect(wait = wait, file = file)
}

#' @rdname collect.query
#' @export
collect.data_request <- function(.data, wait = FALSE, file = NULL){
  collapse(.data) |>
    compute() |>
    collect(wait = wait, file = file)
}

#' @rdname collect.query
#' @export
collect.metadata_request <- function(.data){
  collapse(.data) |>
    compute() |>
    collect()
}

# below old

# if calling `collect()` after `request_files()`
#' @rdname collect.data_request
#' @export
collect.files_request <- function(.data){
  compute(.data) |> 
    collect()
}

# if calling `collect()` after `collapse()` after request_files()`
#' @rdname collect.data_request
#' @export
collect.files_query <- collect.files_request

# if calling `collect()` after `compute()` after `request_files()`
#' @rdname collect.data_request
#' @export
collect.files_response <- function(.data, file = NULL){
  switch(.data$type,
    "distributions" = abort("Not implemented yet"),
    "media" = collect_media_files(.data),
    abort("unrecognised 'type' supplied to `galah_call()`")
  )
}