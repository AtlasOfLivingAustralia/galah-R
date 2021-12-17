#' Return species lists
#'
#' While there are reasons why users may need to check every record meeting their
#' search criteria (i.e. using [atlas_occurrences()()]), a common use case
#' is to simply identify which species occur in a specified region, time period,
#' or taxonomic group. This function returns a `data.frame` with one row
#' per species, and columns giving associated taxonomic information.
#'
#' @inheritParams atlas_occurrences
#' @param refresh_cache `logical`: if set to `TRUE` and 
#' `galah_config(caching = TRUE)` then files cached from a previous query will 
#' be replaced by the current query
#' @return A `data.frame` of matching species. The `data.frame` object 
#' has attributes listing of the user-supplied arguments of the `data_request` 
#' (i.e., taxa, filter, geolocate, columns)
#' @details
#' The primary use case of this function is to extract species-level information
#' given a set of criteria defined by [search_taxa()],
#' [galah_filter()] or [galah_geolocate()]. If the purpose
#' is simply to get taxonomic information that is not restricted by filtering,
#' then [search_taxa()] is more efficient. Similarly, if counts are
#' required that include filter but without returning taxonomic detail, then
#' [atlas_counts()] is more efficient (see examples).
#' @examples
#'
#' # Look up genus "Heleioporus" in the ALA
#' search_taxa("Heleioporus")
#'
#' # Find how many records there are for this genus
#' atlas_counts(search_taxa("Heleioporus"))
#'
#' # Get taxonomic information on all species within this genus
#' # (every row is a species with associated taxonomic data)
#' atlas_species(search_taxa("Heleioporus"))
#' 
#' @export
atlas_species <- function(taxa = NULL, 
                          filter = NULL, 
                          geolocate = NULL,
                          refresh_cache = FALSE) {
  # check whether species download is possible
  species_url <- server_config("species_base_url")

  url <- server_config("records_base_url")
  query <- list()

  if (missing(taxa) & missing(filter) & missing(geolocate)) {
    warning("This query will return a list of all species in the ALA")
  }

  profile <- extract_profile(filter)
  query <- build_query(taxa, filter, geolocate, profile = profile)
  
  query$facets <- "speciesID"
  query$lookup  <- "true"
  
  path <- "occurrences/facets/download"
  cache_file <- cache_filename("species", unlist(query))
  caching <- getOption("galah_config")$caching
  
  if (caching && file.exists(cache_file) && !refresh_cache) {
    return(read_cache_file(cache_file))
  }
  tmp <- tempfile()
  data <- ala_download(url, path = path, params = query,
                       cache_file = tmp)
  
  # overwrite file with fixed names
  names(data) <- rename_columns(names(data), type = "checklist")
  data <- data[,wanted_columns("checklist")]
  
  query <- data_request(taxa, filter, geolocate)
  attr(data, "data_request") <- query
  
  if (caching) {
    write_cache_file(object = data, data_type = "species",
                     cache_file = cache_file)
  }
  return(data)
}
