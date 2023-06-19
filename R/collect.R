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
#' @param what string: what kind of data should be returned. Must be one of
#' `"counts"`, `"species"`, `"occurrences"` or `"media"`.
#' @param type string: what type of data should be returned. For 
#' `what = "counts"`, this can be `"records"` (default) or `"species"`. For 
#' `what = "occurrences"`, this can be `"records"` (default) or `"media"`.
#' For `what = "media"` this should be one or more of `"images"` (default), 
#' `"videos"` and `"sounds"`.
#' @param filesize if `type` is `"media"`, what size of file should be returned?
#' Should be on of `"full"` (default) or `"thumbnail"`
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
collect.data_request <- function(.data, 
                                 what = "counts", 
                                 type, 
                                 filesize,
                                 path = "."
){
  .data <- check_type(.data, type = type, what = what)
  switch(.data$what, 
         "counts" = {compute(.data) |> 
                       collect_counts()},
         "species" = {
           check_login(.data)
           collapse(.data) |>
              collect_species()},
         "occurrences" = {
           if(.data$type == "media"){
             .data <- .data |> 
               select(group = "media")
             compute(.data) |>
               collect_occurrences(wait = TRUE) |>
               collect_occurrences_media()
           }else{
             compute(.data) |> 
               collect_occurrences(wait = TRUE)
           }
         },
         "media" = {
           compute(.data, type = type) |>
             check_media_args(filesize = filesize, path = path) |>
             collect_media()
         }
      )
}

# if calling `collect()` after `collapse()`
#' @rdname collect.data_request
#' @export
collect.data_query <- function(.data, what, type, path){
  .data <- check_type(.data, type, what)
  switch(.data$what,
         "counts" = collect_counts(.data),
         "species" = {
           check_login(.data)
           collect_species(.data)},
         "occurrences" = {
           compute(.data) |>
             collect_occurrences(wait = TRUE)},
         "media" = {
           compute(.data) |>
             check_media_args(filesize = type, path = path) |>
             collect_media()
         }
  )
}

# if calling `collect()` after `compute()`
#' @rdname collect.data_request
#' @param wait logical; should `galah` ping the selected url until computation
#' is complete? Defaults to `FALSE`.
#' @export
collect.data_response <- function(.data,
                                  wait = FALSE,
                                  filesize = "full",
                                  path = "."){
  switch(.data$what,
         "counts" = collect_counts(.data),
         # "species" # doesn't exist, as `compute('species')` is not implemented
         "occurrences" = collect_occurrences(.data, wait),
         "media" = {
           x <- .data |>
             check_media_args(filesize = filesize, path = path) |>
             collect_media()}
  )
}

# if calling `compute()` after `galah_call()` 
#' @rdname collect.data_request
#' @importFrom potions pour
#' @importFrom rlang abort
#' @export
compute.data_request <- function(.data, what, type){
  .data <- check_type(.data, type, what) |>
           collapse()
  switch_compute(.data)
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
  switch(.data$what, 
         "counts" = compute_counts(.data),
         "species" = abort("`compute('species')` does not exist; try `collect('species')`"),
         "occurrences" = {
           check_login(.data)
           compute_occurrences(.data)},
         "media" = {
           check_login(.data)
           compute_media(.data)})   
}

# if calling `collapse()` after `galah_call()`
#' @rdname collect.data_request
#' @export
collapse.data_request <- function(.data, what, type){
  .data <- check_type(.data, type, what)
  switch(.data$what, 
         "counts" = collapse_counts(.data),
         "species" = collapse_species(.data),
         "occurrences" = collapse_occurrences(.data),
         "media" = collapse_media(.data))
}