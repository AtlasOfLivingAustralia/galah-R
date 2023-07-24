#' collapse for type = "species-count"
#' @keywords Internal
#' @param .data an object of class `data_request`
#' @noRd
collapse_species_count <- function(.data){
  if(is_gbif()){
    abort("`count()` is not supported for GBIF with type = 'species'") ## TRUE?
  }else{
    function_name <- "collapse_species_count_atlas"
    arg_names <- names(formals(collapse_species_count_atlas))
  }
  custom_call <- .data[names(.data) %in% arg_names]
  class(custom_call) <- "data_request"
  do.call(function_name, custom_call)
}

#' collapse for counts on LAs
#' @keywords Internal
#' @noRd
collapse_species_count_atlas <- function(identify = NULL, 
                                         filter = NULL, 
                                         geolocate = NULL,
                                         data_profile = NULL,
                                         group_by = NULL, 
                                         limit = 100){
  
  if(!is.null(data_profile)){
    profile <- data_profile$data_profile
  }else{
    profile <- NULL
  }
  query <- build_query(identify, filter, geolocate, profile = profile)
  
  if(is.null(group_by)){
    url <- url_lookup("records_facets") 
    query$flimit <- 1
    query$facets <- species_facets() 
    column <- "count"
    expand <- FALSE
  }else{
    # NOTE: this section not updated to enforce type = "species"
    url <- url_lookup("records_facets")
    facets <- as.list(group_by$name)
    names(facets) <- rep("facets", length(facets))
    query <- c(query, facets, flimit = limit)
    if(length(facets) > 1){
      expand <- TRUE
    }else{
      expand <- FALSE
    }
    column <- "fieldResult"
  }
  
  # aggregate and return
  result <- list(url = url, 
                 headers = build_headers(),
                 query = query, 
                 expand = expand,
                 column = column,
                 type = type)
  class(result) <- "data_query"
  return(result)
}