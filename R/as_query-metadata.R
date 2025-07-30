# These functions are called by `as_query.metadata_request()`

#' Internal function get a tibble of APIs
#' @noRd
#' @keywords Internal
as_query_apis <- function(){
  result <- list(type = "metadata/apis",
                 data = "galah:::node_config")
  class(result) <- "query"
  return(result)
}

#' Internal function to create an assertions query
#' NOTE: API doesn't accept any arguments - could post-filter for search
#' @noRd
#' @keywords Internal
as_query_assertions <- function(){
  if(is_gbif()){
    result <- list(type = "metadata/assertions",
                   data = "galah:::gbif_internal_archived$assertions")
  }else{
    update_needed <- internal_cache_update_needed("assertions")
    if(update_needed){
      result <- list(type = "metadata/assertions",
                     url = url_lookup("metadata/assertions"),
                     headers = build_headers())
    }else{
      result <- list(type = "metadata/assertions",
                     data = "galah:::check_internal_cache()$assertions")      
    }
  }
  class(result) <- "query"
  return(result)
}

#' Internal function to create an atlases query
#' @noRd
#' @keywords Internal
as_query_atlases <- function(){
  result <- list(type = "metadata/atlases",
                 data = "galah:::node_metadata")
  class(result) <- "query"
  return(result)
}

#' Internal function to create a collections query
#' @noRd
#' @keywords Internal
as_query_collections <- function(.query){
  url <- url_lookup("metadata/collections") 
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      url <- httr2::url_parse(url)
      url$query <- list(q = .query$filter$value[1])
      url <- httr2::url_build(url)
    }
  }
  result <- list(type = "metadata/collections",
                 url = url,
                 headers = build_headers()) 
  class(result) <- "query"
  return(result)
}
# NOTE: LA collectory functions do not accept `max` or `offset`
# Therefore they cannot be paginated. GBIF collectory funs can.

#' Internal function to create a datasets query
#' @noRd
#' @keywords Internal
as_query_datasets <- function(.query){
  url <- url_lookup("metadata/datasets") 
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      url <- httr2::url_parse(url)
      url$query <- list(q = .query$filter$value[1])
      url <- httr2::url_build(url)
    }
  }
  result <- list(type = "metadata/datasets",
                 url = url,
                 headers = build_headers()) 
  class(result) <- "query"
  return(result)
}

#' Internal function to create a fields query
#' Note that this is inconsistent with `show_all_fields()` which returns data
#' from multiple APIs
#' @noRd
#' @keywords Internal
as_query_fields <- function(){
  if(is_gbif()){
    result <- list(type = "metadata/fields",
                   data = "galah:::gbif_internal_archived$fields")
  }else{
    update_needed <- internal_cache_update_needed("fields")
    if(update_needed){
      result <- list(type = "metadata/fields",
                     url = url_lookup("metadata/fields"),
                     headers = build_headers())
    }else{
      result <- list(type = "metadata/fields",
                     data = "galah:::check_internal_cache()$fields")      
    }
  }
  class(result) <- "query"
  return(result) 
}

#' Internal function to create a licences query
#' @noRd
#' @keywords Internal
as_query_licences <- function(){
  result <- list(type = "metadata/licences",
                 url = url_lookup("metadata/licences"),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to create a lists query
#' @noRd
#' @keywords Internal
as_query_lists <- function(.query){
  url <- url_lookup("metadata/lists") |>
    httr2::url_parse()
  url$query <- list(max = 10000)
  if(!missing(.query)){
    if(!is.null(.query$slice)){
      url$query <- list(max = .query$slice$slice_n)
    }    
  }
  result <- list(type = "metadata/lists",
                 url = httr2::url_build(url),
                 headers = build_headers(),
                 slot_name = "lists")
  class(result) <- "query"
  return(result) 
}

#' Internal function to create a profiles query
#' @noRd
#' @keywords Internal
as_query_profiles <- function(){
  update_needed <- internal_cache_update_needed("profiles")
  if(update_needed){
    result <- list(type = "metadata/profiles",
                   url = url_lookup("metadata/profiles"),
                   headers = build_headers())    
  }else{
    result <- list(type = "metadata/profiles",
                   data = "galah:::check_internal_cache()$profiles")
  }
  class(result) <- "query"
  return(result)
}

#' Internal function to create a providers query
#' @noRd
#' @keywords Internal
as_query_providers <- function(.query){
  url <- url_lookup("metadata/providers") 
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      url <- httr2::url_parse(url)
      url$query <- list(q = .query$filter$value[1])
      url <- httr2::url_build(url)
    }
  }
  result <- list(type = "metadata/providers",
                 url = url,
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to create a reasons query
#' @noRd
#' @keywords Internal
as_query_reasons <- function(){
  update_needed <- internal_cache_update_needed("reasons")
  if(update_needed){
    result <- list(type = "metadata/reasons",
                   url = url_lookup("metadata/reasons"),
                   headers = build_headers())
  }else{
    result <- list(type = "metadata/reasons",
                   data = "galah:::check_internal_cache()$reasons")
  }
  class(result) <- "query"
  return(result)
}

#' Internal function to create a ranks query
#' @noRd
#' @keywords Internal
as_query_ranks <- function(){
  if(is_gbif()){
    result <- list(type = "metadata/ranks",
                   data = "galah:::gbif_internal_archived$ranks")
  }else{
    result <- list(type = "metadata/ranks",
                   data = "galah:::galah_internal_archived$ranks")
  }
  class(result) <- "query"
  return(result)
}
