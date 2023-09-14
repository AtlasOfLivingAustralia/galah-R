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
#' @name collect.data_request
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
#' @export
collect.data_request <- function(.data, 
                                 wait = FALSE, 
                                 file = NULL){
  .data$type <- check_type(.data$type)
  switch(.data$type,
         "doi" = {compute(.data) |> collect_doi()},
         "media" = {
           collapse(.data) |>
             collect_media_metadata()},
         "occurrences" = {
             compute(.data) |> 
               collect_occurrences(wait = wait, file = file)
         },
         "occurrences-count" = {compute(.data) |> 
             collect_occurrences_count()},
         "species" = {
           collapse(.data) |>
             collect_species(file = file)},
         "species-count" = {compute(.data) |>
             collect_species_count()},
         "taxonomy" = {collapse(.data) |> 
             collect_taxonomy()},
         abort("unrecognised 'type' supplied to `galah_call()`")
      )
}

# if calling `collect()` after `request_data() |> collapse()`
#' @rdname collect.data_request
#' @export
collect.data_query <- function(.data, 
                               wait = TRUE, 
                               file = NULL){
  switch(.data$type,
         "doi" = collect_doi(.data, file = file),
         "media" = collect_media_metadata(.data),
         "occurrences" = {
           compute(.data) |>
             collect_occurrences(wait = wait, file = file)},
         "occurrences-count" = collect_occurrences_count(.data),
         "species-count" = collect_species_count(.data),
         "species" = {
           check_login(.data)
           collect_species(.data, file = file)},
         "taxonomy" = {compute(.data) |> collect_taxonomy()},
         query_API(.data) # q: is this sensible?
  )
}

# if calling `collect()` after `compute()`
#' @rdname collect.data_request
#' @param wait logical; should `galah` ping the selected url until computation
#' is complete? Defaults to `FALSE`.
#' @export
collect.data_response <- function(.data, 
                                  wait = TRUE, 
                                  file = NULL){
  switch(.data$type,
         "doi" = collect_doi(.data, file = file),
         "media" = collect_media_metadata(.data),
         "occurrences" = collect_occurrences(.data, wait = wait, file = file),
         "occurrences-count" = collect_occurrences_count(.data),
         "species" = collect_species(.data, file = file),
         "species-count" = collect_species_count(.data),
         "taxonomy" = collect_taxonomy(.data),
         abort("unrecognised 'type'")
  )
}

# if calling `collect()` after `request_metadata()`
#' @rdname collect.data_request
#' @export
collect.metadata_request <- function(.data){
  compute(.data) |> 
  collect()
}

# if calling `collect()` after `collapse()` after request_metadata()`
#' @rdname collect.data_request
#' @export
collect.metadata_query <- collect.metadata_request

# if calling `collect()` after `compute()` after `request_metadata()`
#' @rdname collect.data_request
#' @export
collect.metadata_response <- function(.data){
  # deal with stored data first
  # This handles types = "atlases", "ranks"
  # It also handles "fields", "assertions" and "profiles" when updates are not required
  if(is.null(.data$url) & !is.null(.data$data)){
    sub("^galah:::", "", .data$data) |>
    str2lang() |>
    eval()
  }else{ # this only happens when .data$url exists, which is our tag for pinging an API
    switch(.data$type, 
           "assertions" = collect_assertions(.data),
           "collections" = collect_collections(.data),
           "datasets" = collect_datasets(.data),
           "fields" = collect_fields(.data),
           "licences" = collect_licences(.data),
           "lists" = collect_lists(.data),
           "profiles" = collect_profiles(.data),
           "providers" = collect_providers(.data),
           "reasons" = collect_reasons(.data),
           "taxa" = collect_taxa(.data),
           "identifiers" = collect_identifiers(.data))
  }
}

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

# if calling `collect()` after `request_values()`
#' @rdname collect.data_request
#' @export
collect.values_request <- function(.data){
  compute(.data) |> 
    collect()
}

# if calling `collect()` after `collapse()` after request_values()`
#' @rdname collect.data_request
#' @export
collect.values_query <- collect.values_request

# if calling `collect()` after `compute()` after `request_values()`
#' @rdname collect.data_request
#' @export
collect.values_response <- function(.data){
  switch(.data$type,
         "collections" = collect_collection_values(.data),
         "datasets" = collect_dataset_values(.data),
         "fields" = collect_field_values(.data),
         "lists" = collect_list_values(.data),
         "profiles" = collect_profile_values(.data),
         "providers" = collect_provider_values(.data),
         "taxa" = collect_taxa_values(.data),
         abort("unrecognised 'type'"))
}