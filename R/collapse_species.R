#' Internal `collapse` function for species lists
#' @keywords Internal
#' @noRd
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
  
  # check whether API exists
  base_url <- url_lookup("records_species")
  
  # build a query
  query <- c(
    build_query(identify, filter, geolocate, profile = data_profile),
    emailNotify = email_notify(),
    sourceTypeId = 2004,
    reasonTypeId = pour("user", "download_reason_id"),
    email = pour("user", "email"), 
    facets = species_facets(),
    lookup = "true")
  
  # build output
  result <- list(
    url = base_url,
    headers = list("User-Agent" = galah_version_string()),
    query = query)
  result$what <- "species"
  class(result) <- "data_query"
  
  return(result)
}