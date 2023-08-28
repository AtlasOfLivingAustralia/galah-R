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
                                             slice = NULL,
                                             arrange = NULL
){
  query <- build_query(identify, 
                       filter, 
                       geolocate, 
                       data_profile = data_profile$data_profile) 
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
    if(is.null(slice)){
      slice <- tibble(slice_n = 30, slice_called = FALSE)
    }
    if(is.null(arrange)){
      arrange <- tibble(variable = "count", direction = "descending")
    }
    slice_arrange <- bind_cols(slice, arrange)
    arrange_list <- slice_arrange |> check_slice_arrange()
    url$query <- c(query, facets, arrange_list)
    result$url <- url_build(url)
    result$expand <- ifelse(length(facets) > 1, TRUE, FALSE)
    result$arrange <- slice_arrange
  }
  
  # aggregate and return
  result$headers <- build_headers()
  class(result) <- "data_query"
  return(result)
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

check_slice_arrange <- function(df){
  if(df$variable == "count"){ # arranged in descending order by default
    if(df$direction == "ascending"){
      list(fsort = "count", flimit = 0)
    }else{
      list(fsort = "count", flimit = df$slice_n)
    }
  }else{ # non-count fields are arranged in ascending order by default
    if(df$direction == "ascending"){
      list(fsort = "index", flimit = df$slice_n)
    }else{
      list(fsort = "index", flimit = 0)
    }
  }
}