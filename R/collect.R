#' Force computation of a database query
#'
#' These functions are borrowed from `dplyr`, and underpin every `atlas_` 
#' function in `galah`. `collapse()` calculates a valid query so it can be 
#' inspected before being sent. `compute()` sends the query so that it can 
#' be calculated server-side in the specified atlas. `collect()` returns the
#' resulting `tibble` once it is complete.
#' 
#' This function has three extensions in `galah`. `collect.data_request()` is
#' designed to be called at the end of a pipe, `collect.data_response()` works
#' after calls to `compute`, and `collect.data_query()` works after calls to 
#' `collapse()`.
#' @name collect.data_request
#' @param .data An object of class `data_request`, `data_query` or 
#' `data_response` 
#' @param wait logical; should `galah` wait for a response? Defaults to FALSE.
#' Only applies for `type = "occurrences"` or `"species"`.
#' @param file (optional) file name, including path if applicable. If not
#' given will default to a path given by `galah_config()$directory`.
#' @return `collect()` returns a `tibble` containing requested data. `compute()`
#' returns an object of class `data_response`. `collapse()` returns an object of 
#' class `data_query`.
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export
collect.data_request <- function(.data, wait = FALSE, file){
  .data$type <- check_type(.data$type)
  switch(.data$type, 
         "occurrences-count" = {compute(.data) |> 
                                collect_counts()},
         "species-count" = {compute(.data) |> 
                            collect_counts()},
         "doi" = collect_doi(.data),
         "species" = {
           collapse(.data) |>
              collect_species(wait = wait, file = file)},
         "occurrences" = {
             compute(.data) |> 
               collect_occurrences(wait = wait, file = file)
         },
         "media" = {
            collapse(.data) |>
              collect_media()},
         abort("unrecognised 'type' supplied to `galah_call()`")
      )
}

# if calling `collect()` after `request_data() |> collapse()`
#' @rdname collect.data_request
#' @export
collect.data_query <- function(.data, wait = FALSE, file){
  switch(.data$type,
         "occurrences-count" = collect_counts(.data),
         # "species-count" = {?},
         "species" = {
           check_login(.data)
           collect_species(wait = wait, file = file)},
         "occurrences" = {
           compute(.data) |>
             collect_occurrences(wait = wait, file = file)},
         "media" = collect_media_metadata(.data),
         query_API
  )
}

# if calling `collect()` after `compute()`
#' @rdname collect.data_request
#' @param wait logical; should `galah` ping the selected url until computation
#' is complete? Defaults to `FALSE`.
#' @export
collect.data_response <- function(.data, wait = FALSE, file){
  switch(.data$type,
         "occurrences-count" = collect_counts(.data),
         "species-count" = collect_counts(.data),
         "species" = collect_species(wait = wait, file = file),
         "occurrences" = collect_occurrences(wait = wait, file = file),
         "media" = collect_media(.data),
         abort("unrecognised 'type'")
         # NOTE: "species" & "doi" have no `compute()` stage
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
collect.metadata_query <- function(.data){
  compute(.data) |> 
  collect()
}

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
           "layers" = collect_layers(.data),
           "licences" = collect_licences(.data),
           "lists" = collect_lists(.data),
           "profiles" = collect_profiles(.data),
           "providers" = collect_providers(.data),
           "reasons" = collect_reasons(.data),
           "taxa" = collect_taxa(.data))
  }
}