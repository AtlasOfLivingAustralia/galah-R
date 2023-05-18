#' Collect data from the selected atlas
#'
#' For `type = "media"` or `"occurrences"`, object must first have been passed 
#' to the specified atlas using `compute()`. For `type = "counts"` this is not 
#' required; in fact `collect()` is synonymous with `count()` in this instance.
#' `r lifecycle::badge("experimental")` 
#' @seealso [atlas_occurrences()]
#' @param .data An object of class `data_response`, created using 
#' [compute.data_request()]
#' @return A `tibble` containing requested data
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export
collect.data_response <- function(.data,
                                  wait = FALSE){
  switch(attr(.data, "type"), 
         "species" = collect_species(.data),
         "occurrences" = collect_occurrences(.data, wait)
         # "media" = collect_media(.data)) # unclear whether this makes sense
         # may need types "media-metadata" and "media-files"
  )
}

#' @rdname collect.data_request
#' @export
collect.data_request <- function(.data){
  switch(.data$type, 
         "counts" = count(.data)
  )
}