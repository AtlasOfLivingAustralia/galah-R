#' capture() for type = "species-count"
#' @keywords Internal
#' @param .query an object of class `data_request`
#' @noRd
capture_species_count <- function(.query,
                                   error_call = rlang::caller_env()){
  if(is_gbif()){
    cli::cli_abort("`count()` is not supported for GBIF with type = 'species'",
                   call = error_call) 
  }else{
    function_name <- "capture_species_count_atlas"
    arg_names <- names(formals(capture_species_count_atlas))
  }
  custom_call <- .query[names(.query) %in% arg_names]
  class(custom_call) <- "data_request"
  do.call(function_name, custom_call)
}

#' collapse for counts on LAs
#' @keywords Internal
#' @noRd
capture_species_count_atlas <- function(identify = NULL, 
                                        filter = NULL, 
                                        geolocate = NULL,
                                        apply_profile = NULL,
                                        group_by = NULL, 
                                        distinct = NULL,
                                        slice_arrange = NULL
){
  # determine facets
  if(is.null(distinct)){
    facet_variable <- species_facets()
  }else{
    facet_variable <- distinct$name[[1]]
  }
  
  # get url
  url <- url_lookup("data/species-count") |> 
    httr2::url_parse()
  query <- build_query(identify, 
                       filter, 
                       geolocate, 
                       apply_profile = apply_profile)
  
  # set behaviour depending on `group_by()`
  if(is.null(group_by)){
    url$query <- c(query,
                   list(flimit = 1, 
                        facets = facet_variable))
    result <- list(type = "data/species-count",
                   url = httr2::url_build(url),
                   headers = build_headers())
  }else{
    facets <- c(as.list(group_by$name),
                facet_variable)
    names(facets) <- rep("facets", length(facets))
    url$query <- c(query,
                   facets,
                   parse_slice_arrange(slice_arrange),
                   list(flimit = -1))
    result <- list(type = "data/species-count",
                   url = httr2::url_build(url),
                   headers = build_headers())
  }
  as_prequery(result)
}
