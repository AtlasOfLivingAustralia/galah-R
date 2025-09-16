# These functions are called by `as_query.metadata_request()`

# Utility functions to build default forms of query

#' Internal function to define a standard query
#' @param query_type the sting used to define that query type; see `show_all_apis()`
#' @noRd
#' @keywords Internal
default_query <- function(query_type){
  list(type = query_type,
       url = url_lookup(query_type),
       headers = build_headers())
}

#' Ditto for cache
#' @noRd
#' @keywords Internal
default_cache <- function(query_type){
  specific_type <- stringr::str_remove(query_type, "^metadata/")
  list(type = query_type,
       data = glue::glue("galah:::retrieve_cache(\"{specific_type}\")")) 
}

#' Modified version of `default_query()` that supports filtering
#' @noRd
#' @keywords Internal
filtered_query <- function(query_type, .query){
  url <- query_type |>
    url_lookup() |>
    httr2::url_parse()
  url$query <- list(q = .query$filter$value[1])
  list(type = query_type,
       url = httr2::url_build(url),
       headers = build_headers()) 
}

# Actual functions called to build those queries

#' Internal function get a tibble of APIs
#' @noRd
#' @keywords Internal
as_query_apis <- function(){
  list(type = "metadata/apis",
       data = "galah:::node_config") |>
    as_query()
}

#' Internal function to create an assertions query
#' NOTE: API doesn't accept any arguments - could post-filter for search
#' @noRd
#' @keywords Internal
as_query_assertions <- function(){
  query_type <- "metadata/assertions"
  if(is_gbif()){
    result <- list(type = query_type,
                   data = "galah:::gbif_internal_archived$assertions")
  }else{
    if(check_if_cache_update_needed("assertions")){
      result <- default_query(query_type)
        
    }else{
      result <- default_cache(query_type)
    }
  }
  as_query(result)
}

#' Internal function to create an atlases query
#' @noRd
#' @keywords Internal
as_query_atlases <- function(){
  list(type = "metadata/atlases",
       data = "galah:::node_metadata") |>
    as_query()
}

#' Internal function to create a collections query
#' @noRd
#' @keywords Internal
as_query_collections <- function(.query){
  # set `type`
  query_type <- "metadata/collections"
  # If `filter()` is supplied, we always need a query
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      result <- filtered_query(query_type, .query)
    }else{
      result <- default_query(query_type)
    }
  # If no `filter()`, check cache instead
  }else{
    if(check_if_cache_update_needed("collections")){
      result <- default_query(query_type)
    }else{
      result <- default_cache(query_type)
    }
  }
  as_query(result)
}
# NOTE: LA collectory functions do not accept `max` or `offset`
# Therefore they cannot be paginated. GBIF collectory funs can.

#' Internal function to create a datasets query
#' @noRd
#' @keywords Internal
as_query_datasets <- function(.query){
  # set `type`
  query_type <- "metadata/datasets"
  # If `filter()` is supplied, we always need a query
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      result <- filtered_query(query_type, .query)
    }else{
      result <- default_query(query_type)
    }
    # If no `filter()`, check cache instead
  }else{
    if(check_if_cache_update_needed("datasets")){
      result <- default_query(query_type)
    }else{
      result <- default_cache(query_type)
    }
  }
  as_query(result)
}

#' Internal function to create a fields query
#' Note that this is inconsistent with `show_all_fields()` which returns data
#' from multiple APIs
#' @noRd
#' @keywords Internal
as_query_fields <- function(){
  query_type <- "metadata/fields"
  if(check_if_cache_update_needed("fields")){
    result <- default_query(query_type)
  }else{
    result <- default_cache(query_type)     
  }
  as_query(result)
}

#' Internal function to create a licences query
#' @noRd
#' @keywords Internal
as_query_licences <- function(){
  query_type <- "metadata/licences"
  if(check_if_cache_update_needed("licences")){
    result <- default_query(query_type)
  }else{
    result <- default_cache(query_type)     
  }
  as_query(result)
}

#' Internal function to create a lists query
#' @noRd
#' @keywords Internal
as_query_lists <- function(.query){
  query_type <- "metadata/lists"
  if(check_if_cache_update_needed("lists")){
    url <- url_lookup(query_type) |>
      httr2::url_parse()
    url$query <- list(max = 10000)
    if(!missing(.query)){
      if(!is.null(.query$slice)){
        url$query <- list(max = .query$slice$slice_n)
      }    
    }
    result <- list(type = query_type,
                   url = httr2::url_build(url),
                   headers = build_headers(),
                   slot_name = "lists")
  }else{
    result <- default_cache(query_type)
  }
  as_query(result)
}

#' Internal function to create a profiles query
#' @noRd
#' @keywords Internal
as_query_profiles <- function(){
  query_type <- "metadata/profiles"
  if(check_if_cache_update_needed("profiles")){
    result <- default_query(query_type)
  }else{
    result <- default_cache(query_type)     
  }
  as_query(result)
}

#' Internal function to create a providers query
#' @noRd
#' @keywords Internal
as_query_providers <- function(.query){
  # set `type`
  query_type <- "metadata/providers"
  # If `filter()` is supplied, we always need a query
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      result <- filtered_query(query_type, .query)
    }else{
      result <- default_query(query_type)
    }
    # If no `filter()`, check cache instead
  }else{
    if(check_if_cache_update_needed("providers")){
      result <- default_query(query_type)
    }else{
      result <- default_cache(query_type)
    }
  }
  as_query(result)
}

#' Internal function to create a reasons query
#' @noRd
#' @keywords Internal
as_query_reasons <- function(){
  query_type <- "metadata/reasons"
  if(check_if_cache_update_needed("reasons")){
    result <- default_query(query_type)
  }else{
    result <- default_cache(query_type)     
  }
  as_query(result)
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
  as_query(result)
}
