#' Internal function to `collapse()` for `type = "species"`
#' @noRd
#' @keywords Internal
collapse_species <- function(q_obj){
  if(is_gbif()){
    result <- collapse_occurrences_gbif(q_obj, 
                                        format = "SPECIES_LIST")
    result$type <- "data/species"
    result
  }else{
    collapse_species_atlas(q_obj)
  }
}

#' calculate the query to be returned for a given living atlas
#' @noRd
#' @keywords Internal
collapse_species_atlas <- function(q_obj){
  # build a query
  query <- c(
    build_query(q_obj$identify, 
                q_obj$filter, 
                q_obj$geolocate, 
                q_obj$data_profile),
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
