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
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @keywords Internal
#' @noRd
collapse_occurrences_count_gbif <- function(identify = NULL, 
                                            filter = NULL, 
                                            group_by = NULL,
                                            limit = 100
                                            ){
  # get relevant information
  url <- url_lookup("records_counts") |> url_parse()
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
  url$query <- query
  # aggregate and return
  result <- list(type = "occurrences-count",
                 url = url_build(url), 
                 headers = build_headers(),
                 slot_name = "count")
  class(result) <- "data_query"
  return(result)
}

#' collapse for counts on LAs
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
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
  result <- list(type = "occurrences-count")
  
  if(is.null(group_by)){
    url <- url_lookup("records_counts") |> url_parse()
    url$query <- c(query, pageSize = 0)
    result$url <- url_build(url)
    result$slot_name <- "totalRecords"
    result$expand <- FALSE
  }else{
    url <- url_lookup("records_facets") |> url_parse()
    facets <- as.list(group_by$name)
    names(facets) <- rep("facets", length(facets))
    url$query <- c(query, facets, flimit = limit)
    result$url <- url_build(url)
    if(length(facets) > 1){
      result$expand <- TRUE
    }else{
      result$expand <- FALSE
    }
    result$return_basic <- TRUE
  }
  
  # aggregate and return
  result$headers <- build_headers()
  class(result) <- "data_query"
  return(result)
}
