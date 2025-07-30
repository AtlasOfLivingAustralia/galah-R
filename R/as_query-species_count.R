#' collapse for type = "species-count"
#' @keywords Internal
#' @param .query an object of class `data_request`
#' @noRd
collapse_species_count <- function(.query){
  if(is_gbif()){
    cli::cli_abort("`count()` is not supported for GBIF with type = 'species'") 
  }else{
    function_name <- "as_query_species_count_atlas"
    arg_names <- names(formals(as_query_species_count_atlas))
  }
  custom_call <- .query[names(.query) %in% arg_names]
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
                                         slice = NULL,
                                         arrange = NULL
){
  url <- url_lookup("data/species-count") |> 
    httr2::url_parse()
  query <- build_query(identify, 
                       filter, 
                       geolocate, 
                       data_profile = data_profile)
  # set behaviour depending on `group_by()`
  if(is.null(group_by)){
    url$query <- c(query,
                   list(flimit = 1, 
                        facets = species_facets()))
    result <- list(type = "data/species-count",
                   url = url_build(url),
                   headers = build_headers(),
                   filter = filter,
                   expand = FALSE)
  }else{
    facets <- c(as.list(group_by$name), species_facets())
    names(facets) <- rep("facets", length(facets))
    if(is.null(slice)){
      slice <- tibble::tibble(slice_n = 30, slice_called = FALSE)
    }
    if(is.null(arrange)){
      arrange <- tibble::tibble(variable = "count", direction = "descending")
    }
    slice_arrange <- dplyr::bind_cols(slice, arrange) 
    arrange_list <- check_slice_arrange(slice_arrange)
    url$query <- c(query, facets, arrange_list)
    result <- list(type = "data/species-count",
                   url = httr2::url_build(url),
                   headers = build_headers(),
                   filter = filter,
                   expand = TRUE,
                   arrange = slice_arrange)
  }
  class(result) <- "query"
  return(result)
}
