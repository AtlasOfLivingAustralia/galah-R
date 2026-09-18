#' capture() for type = "species-count"
#' @keywords Internal
#' @param .query an object of class `data_request`
#' @noRd
capture_species_count <- function(.query,
                                   error_call = rlang::caller_env()){
  if(.query$atlas == "Global"){
    cli::cli_abort("`count()` is not supported for GBIF with type = 'species'",
                   call = error_call) 
  }else{
    capture_species_count_atlas(.query)
  }
}

#' collapse for counts on LAs
#' @keywords Internal
#' @noRd
capture_species_count_atlas <- function(.query){
  .query$type <- "data/species-count"
  # determine facets
  if(is.null(.query$distinct)){
    facet_variable <- species_facets(.query)
  }else{
    facet_variable <- .query$distinct$name[[1]]
  }
  
  # get url
  url <- url_lookup(.query) |> 
    httr2::url_parse()
  query <- build_query(identify = .query$identify, 
                       filter = .query$filter, 
                       geolocate = .query$geolocate, 
                       apply_profile = .query$apply_profile,
                       atlas = .query$atlas)
  
  # set behaviour depending on `group_by()`
  if(is.null(.query$group_by)){
    url$query <- c(query,
                   list(flimit = 1, 
                        facets = facet_variable))
    result <- list(type = .query$type,
                   atlas = .query$atlas,
                   url = httr2::url_build(url),
                   headers = build_headers())
  }else{
    facets <- c(as.list(.query$group_by$name),
                facet_variable)
    names(facets) <- rep("facets", length(facets))
    url$query <- c(query,
                   facets,
                   parse_slice_arrange(.query$slice_arrange),
                   list(flimit = -1))
    result <- list(type =.query$type,
                   atlas = .query$atlas,
                   url = httr2::url_build(url),
                   headers = build_headers())
  }
  as_prequery(result)
}
