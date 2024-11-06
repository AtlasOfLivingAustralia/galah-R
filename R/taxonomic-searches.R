#' @name taxonomic_searches
#' @rdname taxonomic_searches
#' @title Look up taxon information
#' 
#' @description
#' `search_taxa()` allows users to look up taxonomic names, and ensure they are
#' being matched correctly, before downloading data from the specified 
#' organisation.
#' 
#' By default, names are supplied as strings; but users can also specify 
#' taxonomic levels in a search using a `data.frame` or `tibble`. This is useful
#' when the taxonomic _level_ of the name in question needs to be specified,
#' in addition to it's identity. For example, a common method is to use the 
#' `scientificName` column to list a Latinized binomial, but it is also possible
#' to list these separately under `genus` and `specificEpithet` (respectively).
#' A more common use-case is to distinguish between homonyms by listing higher
#' taxonomic units, by supplying columns like `kingdom`, `phylum` or `class`.
#'
#' `search_identifiers()` allows users to look up matching taxonomic names using 
#' their unique `taxonConceptID`. In the ALA, all records are associated with 
#' an identifier that uniquely identifies the taxon to which that record belongs. 
#' Once those identifiers are known, this function allows you to use them to 
#' look up further information on the taxon in question. Effectively this is the 
#' inverse function to [search_taxa()], which takes names and provides 
#' identifiers.
#' 
#' Note that when taxonomic look-up is required within a pipe, the equivalent
#' to `search_taxa()` is \code{\link[=identify.data_request]{identify()}} (or
#' [galah_identify()]). The equivalent to `search_identifiers()` is to use 
#' \code{\link[=filter.data_request]{filter()}} to filter by `taxonConceptId`.
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
#' are already known. \code{\link[=filter.data_request]{filter()}}, 
#' \code{\link[=select.data_request]{select()}},
#' \code{\link[=identify.data_request]{identify()}} and [geolocate()] for ways 
#' to restrict the information returned by [atlas_()] functions.
#' @examples \dontrun{
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
#' }
NULL