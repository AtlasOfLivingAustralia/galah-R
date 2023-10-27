#' Search for taxa with taxonomic identifiers
#'
#' In the ALA, all records are associated with an identifier that uniquely
#' identifies the taxon to which that record belongs. Once those identifiers
#' are known, this function allows you to use them to look up further information
#' on the taxon in question. Effectively this is the inverse function to 
#' [search_taxa()], which takes names and provides identifiers. The resulting
#' `tibble` of taxonomic information can also be passed to [galah_identify()] to
#' filter queries to the specified taxon or taxa.
#'
#' @param ... A vector containing one or more taxonomic identifiers, given as 
#' strings. 
#' @return An object of class `tbl_df`, `data.frame` (aka a tibble) and `ala_id`
#' containing taxonomic information.
#' @seealso [search_taxa()] for how to find species by (scientific) names. 
#' [galah_identify()], [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for other ways to restrict the information returned
#' by [atlas_occurrences()] and related functions.
#' 
#' @examples 
#' # Look up a unique taxon identifier
#' search_identifiers(query = "https://id.biodiversity.org.au/node/apni/2914510")
#' @importFrom dplyr bind_rows
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @export
search_identifiers <- function(...) {
  request_metadata(type = "identifiers") |>
    identify(...) |>
    collect()
}