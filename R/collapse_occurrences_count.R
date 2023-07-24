#' collapse for type = "occurrences-count"
#' @keywords Internal
#' @param .data an object of class `data_request`
#' @noRd
collapse_occurrences_count <- function(.data){
  if(is_gbif()){
    function_name <- "collapse_occurrences_count_gbif"
    arg_names <- names(formals(collapse_occurrences_count_gbif))
  }else{
    function_name <- "collapse_occurrences_count_atlas"
    arg_names <- names(formals(collapse_occurrences_count_atlas))
  }
  custom_call <- .data[names(.data) %in% arg_names]
  class(custom_call) <- "data_request"
  do.call(function_name, custom_call)
}

#' collapse for counts on GBIF
#' @keywords Internal
#' @noRd
collapse_occurrences_count_gbif <- function(identify = NULL, 
                                            filter = NULL, 
                                            group_by = NULL,
                                            limit = 100
                                            ){
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
  result <- list(type = "occurrences-count",
                 url = build_url_internal(url, query), 
                 headers = build_headers(),
                 slot_name = "count")
  class(result) <- "data_query"
  return(result)
}

#' collapse for counts on LAs
#' @keywords Internal
#' @noRd
collapse_occurrences_count_atlas <- function(identify = NULL, 
                                             filter = NULL, 
                                             geolocate = NULL,
                                             data_profile = NULL,
                                             group_by = NULL, 
                                             limit = 100
                                             ){
  query <- build_query(identify, 
                       filter, 
                       geolocate, 
                       profile = data_profile$profile) 
  
  if(is.null(group_by)){
    url <- url_lookup("records_counts")
    query$pageSize <- 0
    slot_name <- "totalRecords"
    expand <- FALSE
  }else{
    url <- url_lookup("records_facets")
    facets <- as.list(group_by$name)
    names(facets) <- rep("facets", length(facets))
    query <- c(query, facets, flimit = limit)
    if(length(facets) > 1){
      expand <- TRUE
    }else{
      expand <- FALSE
    }
    slot_name <- list(1, "fieldResult")
  }
  
  # aggregate and return
  result <- list(type = "occurrences-count",
                 url = build_url_internal(url, query), 
                 headers = build_headers(),
                 slot_name = slot_name,
                 expand = expand)
  class(result) <- "data_query"
  return(result)
}
