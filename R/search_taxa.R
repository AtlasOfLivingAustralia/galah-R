#' Look up taxon information
#'
#' Look up taxonomic names before downloading 
#' data from the ALA, using [atlas_occurrences()], [atlas_species()] or 
#' [atlas_counts()]. Taxon information returned by `search_taxa()` may be
#' passed to [galah_identify()] to provide the `identify` argument of 
#' `atlas_` functions. `search_taxa()` allows users to disambiguate homonyms 
#' (i.e. where the same name refers to taxa in different clades) prior to  
#' downloading data. 
#' 
#' Users can also specify taxonomic levels in a search using a data frame 
#' (tibble). Taxa may be specified using either the `specificEpithet` argument 
#' to designate the second element of a Latin binomial, 
#' or the `scientificName` argument to specify the 
#' scientific name (which may include the subspecific epithet if required). 
#'
#' @param ... : A string of one or more scientific names, separated by commas, 
#' or a data frame specifying taxonomic levels. Note that searches are not 
#' case-sensitive. 
#' 
#' @returns An object of class `tbl_df`, `data.frame` (aka a tibble) and `ala_id`
#' containing taxonomic information.
#' 
#' @seealso [search_identifiers()] for how to get names if taxonomic identifiers 
#' are already known. [galah_identify()], [galah_select()], [galah_filter()], and
#' [galah_geolocate()] for ways to restrict the information returned by
#' [atlas_occurrences()] and related functions. [atlas_taxonomy()] to look 
#' up taxonomic trees.
#' 
#' @examples 
#' # Search using a single string. 
#' # Note that `search_taxa()` is not case sensitive
#' search_taxa("Reptilia")
#'
#' # Search using multiple strings. 
#' # `search_taxa()` will return one row per taxon
#' search_taxa("reptilia", "mammalia")
#' 
#' # Specify taxonomic levels in a tibble using "specificEpiphet"
#' search_taxa(tibble::tibble(
#'   class = "aves", 
#'   family = "pardalotidae", 
#'   genus = "pardalotus", 
#'   specificEpithet = "punctatus"))
#'
#' # Specify taxonomic levels in a tibble using "scientificName"                    
#' search_taxa(tibble::tibble(
#'   family = c("pardalotidae", "maluridae"), 
#'   scientificName = c("Pardalotus striatus striatus", "malurus cyaneus")))
#'
#' @importFrom dplyr rename
#' @importFrom potions pour
#' @importFrom utils adist 
#' @export
search_taxa <- function(...){
  request_metadata() |>
    identify(...) |>
    collect()
}