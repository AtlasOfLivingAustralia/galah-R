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
#' @param query `string`: A vector containing one or more taxonomic
#' identifiers, given as strings. 
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
#' 
#' @export
search_identifiers <- function(query) {

  verbose <- getOption("galah_config")$package$verbose

  if (missing(query)) {
    bullets <- c(
      "Argument `query` is missing, with no default.",
      i = "Did you forget to specify one or more identifiers?"
    )
    abort(bullets, call = caller_env())
  }
  
  matches <- lapply(query, identifier_lookup)
  if(all(unlist(lapply(matches, is.null)))){
    if(galah_config()$package$verbose){
      system_down_message("search_identifiers")
    }
    df <- tibble()
    attr(df, "call") <- "ala_id"
    return(df)
  }else{
    df <- bind_rows(matches) |> tibble()
    attr(df, "call") <- "ala_id"
    return(df) 
  }
}


identifier_lookup <- function(identifier) {
  url <- url_lookup("names_lookup")
  if(getOption("galah_config")$atlas$region == "France"){
    result <- paste0(url, identifier) |> url_GET()
  }else{
    result <- url_GET(url, list(taxonID = identifier)) 
  }
  if (is.null(result)){
    return(NULL)
  }
  if (isFALSE(result$success) && result$issues == "noMatch" && galah_config()$package$verbose) {
    list_invalid_taxa <- glue::glue_collapse(identifier, 
                                             sep = ", ")
    inform(glue("No taxon matches were found for \"{list_invalid_taxa}\"."))
  }
  names(result) <- rename_columns(names(result), type = "taxa")
  result[names(result) %in% wanted_columns("taxa")]
}
