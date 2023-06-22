#' Force computation of a database query
#'
#' These functions are borrowed from `dplyr`, and underpin every `atlas_` 
#' function in `galah`. `collapse()` calculates a valid query so it can be 
#' inspected before being sent. `compute()` sends the query so that it can 
#' be calculated server-side in the specified atlas. `collect()` returns the
#' resulting `tibble` once it is complete.
#' `r lifecycle::badge("experimental")`.
#' 
#' This function has three extensions in `galah`. `collect.data_request()` is
#' designed to be called at the end of a pipe, `collect.data_response()` works
#' after calls to `compute`, and `collect.data_query()` works after calls to 
#' `collapse()`.
#' @name collect.data_request
#' @param .data An object of class `data_request`, `data_query` or 
#' `data_response` 
#' @param filesize if `type` is `"media-files"`, what size of file should be 
#' returned? Should be one of `"full"` (default) or `"thumbnail"`
#' @param path Optional path to where file should be stored. If not given 
#' defaults to `galah_config()$package$path`, which defaults to a temporary 
#' directory.
#' @return `collect()` returns a `tibble` containing requested data. `compute()`
#' returns an object of class `data_response`. `collapse()` returns an object of 
#' class `data_query`.
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export
collect.data_request <- function(.data){
  .data$type <- check_type(.data$type)
  switch(.data$type, 
         "occurrences-count" = {compute(.data) |> 
                                collect_counts()},
         "species-count" = {compute(.data) |> 
                            collect_counts()},
         "doi" = collect_doi(.data),
         "species" = {
           check_login(.data)
           collapse(.data) |>
              collect_species()},
         "occurrences" = {
             compute(.data) |> 
               collect_occurrences(wait = TRUE)
         },
         # NOTE:
         # there is the option here to have:
          # `galah_call(type = "media") |> galah_media() |> collect()` # files
          # `galah_call(type = "occurrences") |> galah_media() |> collect()` # metadata 
         "media-metadata" = {
           collapse(.data) |>
           collect_media_metadata()},
         "media-files" = {
           compute(.data) |>
             collect_media()           
         },
         abort("unrecognised 'type' supplied to `galah_call()`")
      )
}

# if calling `collect()` after `collapse()`
#' @rdname collect.data_request
#' @export
collect.data_query <- function(.data){
  .data$type <- check_type(.data$type)
  switch(.data$type,
         "counts" = collect_counts(.data),
         "species" = {
           check_login(.data)
           collect_species(.data)},
         "occurrences" = {
           compute(.data) |>
             collect_occurrences(wait = TRUE)},
         "media-metadata" = collect_media_metadata(.data),
         "media-files" = {
           compute(.data) |>
             collect_media()
         },
         abort("unrecognised 'type'")
  )
}

# if calling `collect()` after `compute()`
#' @rdname collect.data_request
#' @param wait logical; should `galah` ping the selected url until computation
#' is complete? Defaults to `FALSE`.
#' @export
collect.data_response <- function(.data){
  switch(.data$type,
         "occurrences-count" = collect_counts(.data),
         "species-count" = collect_counts(.data),
         "occurrences" = collect_occurrences(.data, wait),
         "media-files" = collect_media(.data),
         abort("unrecognised 'type'")
         # NOTE: "species" & "doi" have no `compute()` stage
  )
}

# if calling `compute()` after `galah_call()` 
#' @rdname collect.data_request
#' @importFrom potions pour
#' @importFrom rlang abort
#' @export
compute.data_request <- function(.data){
  .data$type <- check_type(.data$type)
  collapse(.data) |>
    switch_compute()
}

# if calling `compute()` after `collapse()`
#' @rdname collect.data_request
#' @export
compute.data_query <- function(.data){
  switch_compute(.data)
}

#' Internal function to determine which type of call to compute
#' @noRd
#' @keywords Internal
switch_compute <- function(.data){
  switch(.data$type, 
         "occurrences-count" = compute_counts(.data),
         "species-count" = compute_counts(.data),
         "doi" = abort(c(
           "`compute()` does not exist for `type = 'doi'`",
           i = "try `collect() instead")),
         "species" = abort(c(
           "`compute()` does not exist for `type = 'species'`",
           i = "try `collect() instead")),
         "occurrences" = {
           check_login(.data)
           compute_occurrences(.data)},
         "media-metadata" = {
           check_login(.data)
           compute_media_metadata(.data)},
         "media-files" = {
           check_login(.data)
           compute_media(.data)})   
}

# if calling `collapse()` after `galah_call()`
#' @rdname collect.data_request
#' @export
collapse.data_request <- function(.data){
  .data$type <- check_type(.data$type)
  switch(.data$type, 
         "occurrences-count" = collapse_counts(.data),
         "species-count" = collapse_counts(.data),
         "doi" = abort(c(
           "`collapse()` does not exist for `type = 'doi'`",
           i = "try `collect() instead")),
         "species" = collapse_species(.data),
         "occurrences" = collapse_occurrences(.data),
         "media-metadata" = collapse_media_metadata(.data),
         "media-files" = collapse_media(.data),
         abort("unrecognised 'type'"))
}