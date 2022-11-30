#' Narrow a query by passing taxonomic identifiers
#'
#' When conducting a search or creating a data query, it is common to identify 
#' a known taxon or group of taxa to narrow down the records or results returned. 
#'
#' `galah_identify()` is used to identify taxa you want returned in a search or 
#' a data query. Users to pass scientific names or taxonomic identifiers
#' with pipes to provide data only for the biological group of interest. 
#' 
#' It is good to use [search_taxa()] and [search_identifiers()] 
#' first to check that the taxa you provide to `galah_identify()` return the 
#' correct results.
#'
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param ... one or more scientific names (if `search = TRUE`) or taxonomic 
#'   identifiers (if `search = FALSE`); or an object of class `ala_id` (from
#'   `search_taxa`).
#' @param search (logical); should the results in question be passed to
#'   `search_taxa`?
#' @return A tibble containing identified taxa.
#' @seealso  [galah_identify()], with which this function is synonymous; 
#' [search_taxa()] to find identifiers from scientific names;
#' [search_identifiers()] for how to get names if taxonomic identifiers 
#' are already known.
#' @export
identify.data_request <- function(x, ..., search = TRUE){
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
  update_galah_call(x, identify = parse_identify(dots, search))
}