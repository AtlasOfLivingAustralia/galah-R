#' Species lists
#'
#' While there are reasons why users may need to check every record meeting their
#' search criteria (i.e. using \code{\link{ala_occurrences}}), a common use case
#' is to simply identify which species occur in a specified region, time period,
#' or taxonomic group. This function returns a \code{data.frame} with one row
#' per species, and columns giving associated taxonomic information.
#'
#' @inheritParams ala_occurrences
#' @param refresh_cache \code{logical}: if set to `TRUE` and 
#' `galah_config(caching = TRUE)` then files cached from a previous query will 
#' be replaced by the current query
#' @return A \code{data.frame} of matching species. The \code{data.frame} object 
#' has attributes listing of the user-supplied arguments of the \code{data_request} 
#' (i.e., taxa, filters, locations, columns)
#' @details
#' The primary use case of this function is to extract species-level information
#' given a set of criteria defined by \code{\link{select_taxa}()},
#' \code{\link{select_filters}()} or \code{\link{select_locations}()}. If the purpose
#' is simply to get taxonomic information that is not restricted by filtering,
#' then \code{\link{select_taxa}()} is more efficient. Similarly, if counts are
#' required that include filters but without returning taxonomic detail, then
#' \code{\link{ala_counts}()} is more efficient (see examples).
#' @examples \dontrun{
#'
#' # Lookup genus "Heleioporus" in the ALA
#' select_taxa("Heleioporus")
#'
#' # How many records are there for this genus?
#' ala_counts(select_taxa("Heleioporus"))
#' # or equivalently:
#' select_taxa("Heleioporus", counts = TRUE)
#'
#' # How best to get taxonomic info on species within this genus?
#' # also includes a row for genus (i.e. not just species)
#' select_taxa("Heleioporus", children = TRUE)
#' # returns counts by species, but no taxonomic information
#' ala_counts(select_taxa("Heleioporus"), group_by = "species")
#' # every row is a species with associated taxonomic data
#' ala_species(select_taxa("Heleioporus"))
#' }
#' @export
ala_species <- function(taxa = NULL, filters = NULL, locations = NULL,
                        refresh_cache = FALSE) {
  # check whether species download is possible
  species_url <- server_config("species_base_url")

  url <- server_config("records_base_url")
  query <- list()

  if (missing(taxa) & missing(filters) & missing(locations)) {
    warning("This query will return a list of all species in the ALA")
  }

  profile <- extract_profile(filters)
  query <- build_query(taxa, filters, locations, profile = profile)
  
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
  
  query <- data_request(taxa, filters, locations)
  attr(data, "data_request") <- query
  
  if (caching) {
    write_cache_file(object = data, data_type = "species",
                     cache_file = cache_file)
  }
  return(data)
}