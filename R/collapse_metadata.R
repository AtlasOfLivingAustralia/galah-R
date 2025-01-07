#' Internal function to `collapse()` apis
#' @noRd
#' @keywords Internal
collapse_apis <- function(){
  result <- list(type = "metadata/apis",
                 data = "galah:::node_config")
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` assertions
#' NOTE: API doesn't accept any arguments - could post-filter for search
#' @noRd
#' @keywords Internal
collapse_assertions <- function(){
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

#' Internal function to `collapse()` atlases
#' @noRd
#' @keywords Internal
collapse_atlases <- function(){
  result <- list(type = "metadata/atlases",
                 data = "galah:::node_metadata")
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` collections
#' @importFrom httr2 url_parse
#' @noRd
#' @keywords Internal
collapse_collections <- function(.query){
  url <- url_lookup("metadata/collections") 
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      url <- url_parse(url)
      url$query <- list(q = .query$filter$value[1])
      url <- url_build(url)
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

#' Internal function to `collapse()` datasets
#' @noRd
#' @keywords Internal
collapse_datasets <- function(.query){
  url <- url_lookup("metadata/datasets") 
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      url <- url_parse(url)
      url$query <- list(q = .query$filter$value[1])
      url <- url_build(url)
    }
  }
  result <- list(type = "metadata/datasets",
                 url = url,
                 headers = build_headers()) 
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` distributions
#' @noRd
#' @keywords Internal
collapse_distributions_metadata <- function(.query){
  url <- url_lookup("metadata/distributions")
  result <- list(type = "metadata/distributions",
                 url = url,
                 headers = build_headers()) 
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` fields
#' Note that this is inconsistent with `show_all_fields()` which returns data
#' from multiple APIs
#' @noRd
#' @keywords Internal
collapse_fields <- function(){
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

#' Internal function to `collapse()` identifiers
#' @noRd
#' @keywords Internal
collapse_identifiers <- function(.query){
  if(is.null(.query$filter)){
    url_list <- url_lookup("metadata/identifiers")
    names(url_list) <- "no-name-supplied"
  }else{
    base_url <- url_lookup("metadata/identifiers") |>
      url_parse()
    search_terms <- .query$filter$value
    query <- as.list(search_terms)
    # create query urls
    urls <- lapply(query,
                   function(a, base_url){
                     names(a) <- "taxonID"
                     base_url$query <- as.list(a)
                     url_build(base_url)
                   },
                   base_url = base_url) |>
      unlist()
  }
  # build object and return
  result <- list(type = "metadata/identifiers",
                 url = tibble(url = urls, 
                              search_term = search_terms),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` licences
#' @noRd
#' @keywords Internal
collapse_licences <- function(){
  result <- list(type = "metadata/licences",
                 url = url_lookup("metadata/licences"),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` lists
#' @noRd
#' @keywords Internal
collapse_lists <- function(.query){
  url <- url_lookup("metadata/lists") |>
    url_parse()
  url$query <- list(max = 10000)
  if(!missing(.query)){
    if(!is.null(.query$slice)){
      url$query <- list(max = .query$slice$slice_n)
    }    
  }
  result <- list(type = "metadata/lists",
                 url = url_build(url),
                 headers = build_headers(),
                 slot_name = "lists")
  class(result) <- "query"
  return(result) 
}

#' Internal function to `collapse()` profiles
#' @noRd
#' @keywords Internal
collapse_profiles <- function(){
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

#' Internal function to `collapse()` providers
#' @noRd
#' @keywords Internal
collapse_providers <- function(.query){
  url <- url_lookup("metadata/providers") 
  if(is_gbif() & !missing(.query)){
    if(!is.null(.query$filter)){
      url <- url_parse(url)
      url$query <- list(q = .query$filter$value[1])
      url <- url_build(url)
    }
  }
  result <- list(type = "metadata/providers",
                 url = url,
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` reasons
#' @noRd
#' @keywords Internal
collapse_reasons <- function(){
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

#' Internal function to `collapse()` ranks
#' @noRd
#' @keywords Internal
collapse_ranks <- function(){
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
