#' Internal function to `collapse()` for `type = "species"`
#' @noRd
#' @keywords Internal
collapse_species <- function(.query){
  if(is_gbif()){
    result <- collapse_occurrences_gbif(.query, 
                                        format = "SPECIES_LIST")
    result$type <- "data/species"
    result
  }else{
    collapse_species_atlas(.query)
  }
}

#' calculate the query to be returned for a given living atlas
#' @noRd
#' @keywords Internal
collapse_species_atlas <- function(.query){
  # build a query
  query <- c(
    build_query(.query$identify, 
                .query$filter, 
                .query$geolocate, 
                .query$data_profile),
    emailNotify = email_notify(),
    sourceTypeId = 2004,
    reasonTypeId = pour("user", "download_reason_id"),
    email = pour("user", "email"), 
    facets = species_facets(),
    lookup = "true")
  # build url
  url <- url_lookup("data/species") |> 
    url_parse()
  url$query <- query
  # build output
  result <- list(
    type = "data/species",
    url = url_build(url),
    headers = build_headers(),
    download = TRUE)
  class(result) <- "query"
  result
}
