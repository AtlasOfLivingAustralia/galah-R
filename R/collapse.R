#' collapse for galah
#' 
#' The `collapse()` function is ported from `dplyr`, and is used to convert 
#' a piped query into an object that can be evaluated by `url_GET()` and related
#' methods. This is used internally for constructing actionable queries, but
#' is exported here for debugging purposes.
#' @param .data An object of class `data_request`.
#' @param what string giving the kind of data query to construct.
#' @export
collapse.data_request <- function(.data, what){
  check_type(what)
  switch(what, 
         "counts" = collapse_counts(.data),
         "species" = collapse_species(.data),
         "occurrences" = collapse_occurrences(.data),
         "media" = collapse_media(.data))
}