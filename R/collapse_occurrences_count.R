#' collapse for type = "occurrences-count"
#' @keywords Internal
#' @param .query an object of class `data_request`
#' @noRd
collapse_occurrences_count <- function(.query){
  if(is_gbif()){
    function_name <- "collapse_occurrences_count_gbif"
    arg_names <- names(formals(collapse_occurrences_count_gbif))
  }else{
    function_name <- "collapse_occurrences_count_atlas"
    arg_names <- names(formals(collapse_occurrences_count_atlas))
  }
  custom_call <- .query[names(.query) %in% arg_names]
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
                       data_profile = data_profile) 
  # set behaviour depending on `group_by()`
  if(is.null(group_by)){
    result <- list(type = "data/occurrences-count")
    url <- url_lookup("data/occurrences-count") |> 
      url_parse()
    url$query <- c(query, pageSize = 0)
    result$url <- url_build(url)
    result$slot_name <- "totalRecords"
    result$expand <- FALSE
  }else{
    result <- list(type = "data/occurrences-count-groupby")
    url <- url_lookup("data/occurrences-count-groupby") |> 
      url_parse()
    facets <- as.list(group_by$name)
    names(facets) <- rep("facets", length(facets))
    if(is.null(slice)){
      # limits to 10,000 rows
      # TODO: This should ultimately be set by `slice` or `atlas_counts(limit = )`, not internally.
      #       Will need updating to avoid hidden limit setting here & in `compute_occurrences_count()`
      slice <- tibble(slice_n = 1e4, slice_called = FALSE) 
    }
    if(is.null(arrange)){
      arrange <- tibble(variable = "count", direction = "descending")
    }
    
    slice_arrange <- bind_cols(slice, arrange) 
    arrange_list <- check_slice_arrange(slice_arrange)
    url$query <- c(query, facets, arrange_list)
    result$url <- url_build(url)
    result$expand <- ifelse(length(facets) > 1, TRUE, FALSE)
    result$arrange <- slice_arrange
  }
  
  # aggregate and return
  result$headers <- build_headers()
  class(result) <- "query"
  return(result)
}

#' collapse for counts on GBIF
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @keywords Internal
#' @noRd
collapse_occurrences_count_gbif <- function(identify = NULL, 
                                            filter = NULL,
                                            geolocate = NULL,
                                            group_by = NULL,
                                            slice = NULL
                                            ){
  # get relevant information
  if(is.null(group_by)){
    url <- url_lookup("data/occurrences-count") |> 
      url_parse()
    url$query <- c(
      build_query_gbif(identify, filter, geolocate),
      limit = 0)
    result <- list(
      type = "data/occurrences-count",
      url = url_build(url),
      slot_name = "count",
      expand = FALSE)
  # add facets
  }else{
    result <- list(type = "data/occurrences-count-groupby")
    url <- url_lookup("data/occurrences-count") |> 
      url_parse()
    facets <- as.list(group_by$name)
    names(facets) <- rep("facet", length(facets))
    if(is.null(slice)){
      slice <- tibble(slice_n = 30, slice_called = FALSE)
    }
    url$query <- c(build_query_gbif(identify, filter, geolocate),
                   limit = 0,
                   facets,
                   facetLimit = slice$slice_n)
    result$url <- url_build(url)
    result$expand <- ifelse(length(facets) > 1, TRUE, FALSE)
  }
  # aggregate and return
  result$headers <- build_headers()
  class(result) <- "query"
  result
}

#' Internal function to check `slice` and `arrange` for counts
#' @keywords Internal
#' @noRd
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
