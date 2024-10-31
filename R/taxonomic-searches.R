#' @name taxonomic_searches
#' @rdname taxonomic_searches
#' @title Look up taxon information
#' 
#' @description
#' `search_taxa()` allows users to look up taxonomic names before downloading 
#' data from the ALA (using [atlas_occurrences()], [atlas_species()] or 
#' [atlas_counts()]). Taxon information returned by `search_taxa()` may be
#' passed to [galah_identify()] as the `identify` argument of 
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
#' `search_identifiers()` allows users to look up matching taxonomic names using 
#' their unique `taxonConceptID`. In the ALA, all records are associated with 
#' an identifier that uniquely identifies the taxon to which that record belongs. 
#' Once those identifiers
#' are known, this function allows you to use them to look up further information
#' on the taxon in question. Effectively this is the inverse function to 
#' [search_taxa()], which takes names and provides identifiers. The resulting
#' `tibble` of taxonomic information can also be passed to [galah_identify()] to
#' filter queries to the specified taxon or taxa.
#' 
#' @details
#' `search_taxa()` returns the taxonomic match of a supplied text string, along 
#' with the following information:
#'   *  `search_term`: The search term used by the user. When multiple search 
#'   terms are provided in a tibble, these are displayed in this column, 
#'   concatenated using `_`.
#'   *  `scientific_name`: The taxonomic name matched to the provided search 
#'   term, to the lowest identified taxonomic rank.
#'   *  `taxon_concept_id`: The unique taxonomic identifier.
#'   *  `rank`: The taxonomic rank of the returned result.
#'   *  `match_type`: (ALA only) The method of name matching used by the name 
#'   matching service. More information can be found on the 
#'   [name matching github repository](https://github.com/AtlasOfLivingAustralia/ala-name-matching?tab=readme-ov-file#understanding-the-name-matching-algorithm).
#'   *  `issues`: Any errors returned by the name matching service 
#'   (e.g. homonym, indeterminate species match). More information can be found 
#'   on the [name matching github repository](https://github.com/AtlasOfLivingAustralia/ala-name-matching?tab=readme-ov-file#error-types).
#'   *  `taxonomic names` (e.g. `kingdom`, `phylum`, `class`, `order`, 
#'   `family`, `genus`)
#' 
#' 
#' @seealso [search_all()] for how to get names if taxonomic identifiers 
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
#' # Search using more detailed strings with authorship information
#' search_taxa("Acanthocladium F.Muell")
#' 
#' # Specify taxonomic levels in a tibble using "specificEpithet"
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
#' # Look up a unique taxon identifier
#' search_identifiers(query = "https://id.biodiversity.org.au/node/apni/2914510")
NULL
#' NULL