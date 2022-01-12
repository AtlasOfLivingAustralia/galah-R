#' Taxonomic identifiers
#'
#' In the ALA, all records are associated with an identifier that uniquely
#' identifies the taxon to which that record belongs. Once those identifiers
#' are known, this function allows you to use them to look up further information
#' on the taxon in question. Effectively this is the inverse function to 
#' [search_taxa()], which takes names and provides identifiers. The resulting
#' `data.frame` of taxonomic information can also be passed directly to
#' `atlas_` functions to filter records to the specified taxon or taxa.
#'
#' @param identifier `string`: A vector containing one or more taxonomic
#' identifiers, given as strings. 
#' @return An object of class `tbl_df`, `data.frame` (aka a tibble) and `ala_id`
#' containing taxonomic information.
#' @seealso [search_taxa()] for how to find species by (scientific) names. 
#' [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for other ways to restrict the information returned
#' by [atlas_occurrences()] and related functions.
#' @examples
#' \dontrun{
#' # Look up a unique taxon identifier
#' find_taxa(identifier = "https://id.biodiversity.org.au/node/apni/2914510")
#' }
#' 
#' @export
find_taxa <- function(identifier) {

  verbose <- getOption("galah_config")$verbose

  if (getOption("galah_config")$atlas != "Australia") {
    international_atlas <- getOption("galah_config")$atlas
    bullets <- c(
      "`find_taxa` only provides information on Australian taxonomy.",
      i = glue("To search taxonomy for {international_atlas} use `taxize`."),
      i = "See vignette('international_atlases' for more information."
    )
    abort(bullets, call = caller_env())
  }

  if (missing(identifier)) {
    bullets <- c(
      "Argument `identifier` is missing, with no default.",
      i = "Did you forget to specify one or more identifiers?"
    )
    abort(bullets, call = caller_env())
  }
  
  matches <- lapply(identifier, identifier_lookup)
  if(all(unlist(lapply(matches, is.null)))){
    if(galah_config()$verbose){
      bullets <- c(
        "Calling the API failed for `find_taxa`.",
        i = "This might mean that the ALA system is down. Double check that your query is correct."
      )
      inform(bullets)
    }
    return(set_galah_object_class(class = "ala_id"))
  }else{ 
    set_galah_object_class(
      rbindlist(matches, fill = TRUE), 
      class = "ala_id") 
  }
}


identifier_lookup <- function(identifier) {
  taxa_url <- server_config("name_matching_base_url")
  result <- atlas_GET(taxa_url, "/api/getByTaxonID", list(taxonID = identifier))
  if (is.null(result)){return(NULL)}
  if (isFALSE(result$success) && result$issues == "noMatch" && galah_config()$verbose) {
    list_invalid_taxa <- glue::glue_collapse(identifier, 
                                             sep = ", ")
    warn(glue("No taxon matches were found for \"{list_invalid_taxa}\"."))
  }
  names(result) <- rename_columns(names(result), type = "taxa")
  result[names(result) %in% wanted_columns("taxa")]
}