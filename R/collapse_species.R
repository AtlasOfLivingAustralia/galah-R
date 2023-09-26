#' Internal function to `collapse()` for `type = "species"`
#' @noRd
#' @keywords Internal
collapse_species <- function(.data){
  if(is_gbif()){
    function_name <- "collapse_occurrences_gbif"
    .data$format <- "SPECIES_LIST"
    arg_names <- names(formals(collapse_occurrences_gbif))
  }else{
    function_name <- "collapse_species_atlas"
    arg_names <- names(formals(collapse_species_atlas))
  }
  custom_call <- .data[names(.data) %in% arg_names]
  class(custom_call) <- "data_request"
  request <- do.call(function_name, custom_call)
  return(request)
}


#' calculate the query to be returned for a given living atlas
#' @noRd
#' @keywords Internal
collapse_species_atlas <- function(identify = NULL,
                                   filter = NULL,
                                   geolocate = NULL,
                                   data_profile = NULL){
  # build a query
  query <- c(
    build_query(identify, filter, geolocate, profile = data_profile),
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
  
  return(result)
}