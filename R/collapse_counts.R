#' collapse for counts
#' @keywords Internal
#' @param .data an object of class `data_request`
#' @noRd
collapse_counts <- function(.data){
  # choose behavior depending on whether we are calling LAs or GBIF
  if(is_gbif()){
    function_name <- "collapse_counts_gbif"
    arg_names <- names(formals(collapse_counts_gbif))
  }else{
    function_name <- "collapse_counts_atlas"
    arg_names <- names(formals(collapse_counts_atlas))
  }
  custom_call <- .data[names(.data) %in% arg_names]
  class(custom_call) <- "data_request"
  
  return(do.call(function_name, custom_call))
}

#' collapse for counts on GBIF
#' @keywords Internal
#' @noRd
collapse_counts_gbif <- function(identify = NULL, 
                                 filter = NULL, 
                                 group_by = NULL, 
                                 type = "records",
                                 limit = 100){
  
  if(type != "records"){
    abort("GBIF only supports `type = 'records'")
  }
  
  # get relevant information
  url <- url_lookup("records_counts")
  query <- build_query(identify, filter)

  # add facets
  query$limit <- 0
  if(!is.null(group_by)){
    if(nrow(group_by) == 1){
      query$facet <- group_by$name
      query$facetLimit <- limit
    }else{
      query$groups <- list(fields = group_by$name,
                           limit = limit,
                           expand = attr(group_by, "expand"))
    }
  }
    
  # aggregate and return
  result <- list(url = url, query = query, column = "count")
  result$type <- "occurrences-counts"
  class(result) <- "data_query"
  return(result)
}

#' collapse for counts on LAs
#' @keywords Internal
#' @noRd
collapse_counts_atlas <- function(identify = NULL, 
                                  filter = NULL, 
                                  geolocate = NULL,
                                  data_profile = NULL,
                                  group_by = NULL, 
                                  limit = 100,
                                  type = "occurrences-count"){
  
  if(!is.null(data_profile)){
    profile <- data_profile$data_profile
  }else{
    profile <- NULL
  }
  query <- build_query(identify, filter, geolocate, profile = profile)
  
  if(is.null(group_by)){
    if(type == "species-count"){
      url <- url_lookup("records_facets") 
      query$flimit <- 1
      query$facets <- species_facets() 
      column <- "count"
    }else{
      url <- url_lookup("records_counts")
      query$pageSize <- 0
      column <- "totalRecords"
    }
    expand <- FALSE
  }else{
    # Q: what if type == "species" here?
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
                 query = query, 
                 expand = expand,
                 column = column,
                 type = type)
  class(result) <- "data_query"
  return(result)
}