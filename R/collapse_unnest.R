#' Internal function to run `collapse()` for `request_values(type = "fields")`
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @noRd
#' @keywords Internal
collapse_fields_unnest <- function(q_obj){
  if(is_gbif()){
    url <- url_lookup("values/fields") |> 
      url_parse()
    url$query <- list(
      facet = q_obj$filter$value[1], 
      limit = 0, 
      facetLimit = 10^4) # FIXME: integrate with `slice_head()`
  }else{
    url <- url_lookup("metadata/fields-unnest") |> 
      url_parse()
    url$query <- list(
      facets = q_obj$filter$value[1], 
      facetLimit = 10^4)
  }
  result <- list(
    type = "metadata/fields-unnest",
    url = url_build(url))
  class(result) <- "query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "lists")`
#' @noRd
#' @keywords Internal
collapse_lists_unnest <- function(q_obj){
  result <- list(
    type = "metadata/lists-unnest",
    url = url_lookup("metadata/lists-unnest",
                     list_id = q_obj$filter$value[1]))
  class(result) <- "query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "profiles")`
#' @noRd
#' @keywords Internal
collapse_profiles_unnest <- function(q_obj){
  result <- list(
    type = "metadata/profiles-unnest",
    url = url_lookup("metadata/profiles-unnest", 
                     profile = q_obj$filter$value[1]))
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` for `type = "taxonomy"`
#' @importFrom rlang abort
#' @importFrom utils URLencode
#' @noRd
#' @keywords Internal
collapse_taxa_unnest <- function(q_obj){
  if(!is.null(q_obj$filter)){
    id <- q_obj$filter$value[1]
  }else if(!is.null(q_obj$identify)){
    id <- "`TAXON_PLACEHOLDER`"
  }
  result <- list(type = "metadata/taxa-unnest",
                 url = url_lookup("metadata/taxa-unnest", id = id),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}
