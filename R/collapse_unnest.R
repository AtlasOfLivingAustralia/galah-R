#' Internal function to run `collapse()` for 
#' `request_metadata(type = "fields") |> unnest()`
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @noRd
#' @keywords Internal
collapse_fields_unnest <- function(.query){
  url <- url_lookup("metadata/fields-unnest") |> 
    url_parse()
  if(is_gbif()){
    url$query <- list(
      limit = 0,
      facet = .query$filter$value[1], # note: facet (singular), not facets (plural)
      facetLimit = 10^4)    
  }else{
    url$query <- list(
      facets = .query$filter$value[1],
      facetLimit = 10^4)   
  }
  result <- list(
    type = "metadata/fields-unnest",
    url = url_build(url))
  class(result) <- "query"
  return(result)
}

#' Internal function to run `collapse()` for 
#' `request_metadata(type = "lists") |> unnest()`
#' @noRd
#' @keywords Internal
collapse_lists_unnest <- function(.query){
  result <- list(
    type = "metadata/lists-unnest",
    url = url_lookup("metadata/lists-unnest",
                     list_id = .query$filter$value[1]))
  class(result) <- "query"
  return(result)
}

#' Internal function to run `collapse()` for 
#' `request_metadata(type = "profiles") |> unnest()`
#' @noRd
#' @keywords Internal
collapse_profiles_unnest <- function(.query){
  result <- list(
    type = "metadata/profiles-unnest",
    url = url_lookup("metadata/profiles-unnest", 
                     profile = .query$filter$value[1]))
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @importFrom rlang abort
#' @importFrom utils URLencode
#' @noRd
#' @keywords Internal
collapse_taxa_unnest <- function(.query){
  if(!is.null(.query$filter)){
    id <- .query$filter$value[1]
  }else if(!is.null(.query$identify)){
    id <- "`TAXON_PLACEHOLDER`"
  }
  result <- list(type = "metadata/taxa-unnest",
                 url = url_lookup("metadata/taxa-unnest", id = id),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}
