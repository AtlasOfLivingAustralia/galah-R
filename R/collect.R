#' Collect data from the selected atlas
#'
#' For `type = "media"` or `"occurrences"`, object must first have been passed 
#' to the specified atlas using `compute()`. For `type = "counts"` this is not 
#' required; in fact `collect()` is synonymous with `count()` in this instance.
#' `r lifecycle::badge("experimental")`.
#' 
#' This function has three extensions in `galah`. `collect.data_request()` is
#' designed to be called at the end of a pipe, `collect.data_response()` works
#' after calls to `compute`, and `collect.data_query()` works after calls to 
#' `collapse()`.
#' @seealso [atlas_occurrences()]
#' @param .data An object of class `data_response`, created using 
#' [compute.data_request()]
#' @param what string: what kind of data should be returned. Must be one of
#' `"counts"`, `"species"`, `"occurrences"` or `"media"`.
#' @return A `tibble` containing requested data
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

#' Collect data from class `data_query`, returned by `collapse.data_request()`
#' @rdname collect.data_query
#' @export
collect.data_query <- function(.data){
  switch(.data$what, 
         "counts" = collect_counts(.data),
         "occurrences" = {collect_occurrences(
           compute(.data, what = "occurrences"), 
           wait = TRUE)}
  )
}

#' Collect data from class `data_response`, returned by `compute.data_request()`
#' @rdname collect.data_request
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