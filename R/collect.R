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
#' @return `collect()` returns a `tibble` containing requested data. `compute()`
#' returns an `data_response`. `collapse()` returns an object of class 
#' `data_query`.
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export
collect.data_request <- function(.data, what){
  switch(what, 
         "counts" = collect_counts(compute(.data, what = "counts")),
         "species" = {collect_species(compute(.data, what = "species"),
                              wait = TRUE)},
         "occurrences" = {collect_occurrences(
                            compute(.data, what = "occurrences"), 
                            wait = TRUE)}
  )
}

#' @rdname collect.data_request
#' @export
collect.data_query <- function(.data){
  switch(.data$what, 
         "counts" = collect_counts(.data),
         "occurrences" = {collect_occurrences(
           compute(.data, what = "occurrences"), 
           wait = TRUE)}
  )
}

#' @rdname collect.data_request
#' @param wait logical; should `galah` ping the selected url until computation
#' is complete? Defaults to `FALSE`.
#' @export
collect.data_response <- function(.data,
                                  wait = FALSE){
  switch(.data$what,
         "counts" = collect_counts(.data),
         "species" = collect_species(.data),
         "occurrences" = collect_occurrences(.data, wait) # note: stored as `attr(.data, "type")`
         # "media" = collect_media(.data)) # unclear whether this makes sense
         # may need types "media-metadata" and "media-files"
  )
}

#' @rdname collect.data_request
#' @importFrom potions pour
#' @importFrom rlang abort
#' @export
compute.data_request <- function(.data, what){
  check_type(what)
  .data <- collapse(.data, what)
  switch_compute(.data, what)
}

#' @rdname collect.data_request
#' @export
compute.data_query <- function(.data){
  switch_compute(.data, what = .data$what)
}

#' Internal function to determine which type of call to compute
#' @noRd
#' @keywords Internal
switch_compute <- function(.data, what){
  if(what != "counts"){
    check_login(.data)
  }
  switch(what, 
         "counts" = compute_counts(.data),
         "species" = compute_species(.data),
         "occurrences" = compute_occurrences(.data),
         "media" = compute_media(.data))   
}

#' @rdname collect.data_request
#' @export
collapse.data_request <- function(.data, what){
  check_type(what)
  switch(what, 
         "counts" = collapse_counts(.data),
         "species" = collapse_species(.data),
         "occurrences" = collapse_occurrences(.data),
         "media" = collapse_media(.data))
}