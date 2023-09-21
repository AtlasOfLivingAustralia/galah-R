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
    result <- list(type = "metadata/assertions",
                   url = url_lookup(method = "metadata", 
                                    type = "assertions"),
                   headers = build_headers())
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
#' @noRd
#' @keywords Internal
collapse_collections <- function(){
  result <- list(type = "metadata/collections",
                 url = url_lookup(method = "metadata",
                                  type = "collections"),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` datasets
#' @noRd
#' @keywords Internal
collapse_datasets <- function(){
  result <- list(type = "metadata/datasets",
                 url = url_lookup(method = "metadata",
                                  type = "datasets"),
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
    result <- list(type = "metadata/fields",
                   url = url_lookup(method = "metadata", 
                                    type = "fields"),
                   headers = build_headers())
  }
  class(result) <- "query"
  return(result) 
}

#' Internal function to `collapse()` licences
#' @noRd
#' @keywords Internal
collapse_licences <- function(){
  result <- list(type = "metadata/licences",
                 url = url_lookup(method = "metadata",
                                  type = "licences"),
                 headers = build_headers())
  class(result) <- "query"
  return(result) 
}

#' Internal function to `collapse()` lists
#' Note: likely to require pagination, and therefore a `compute()` stage
#' to calculate how many urls are needed
#' @noRd
#' @keywords Internal
collapse_lists <- function(){
  result <- list(type = "metadata/lists",
                 url = url_lookup(method = "metadata",
                                  type = "lists"),
                 headers = build_headers(),
                 slot_name = "lists")
  class(result) <- "query"
  return(result) 
}

#' Internal function to `collapse()` profiles
#' @noRd
#' @keywords Internal
collapse_profiles <- function(){
  update_needed <- internal_cache_update_needed("show_all_profiles")
  if(update_needed){
    result <- list(type = "metadata/profiles",
                   url = url_lookup(method = "metadata", 
                                    type = "profiles"),
                   headers = build_headers())    
  }else{
    result <- list(type = "metadata/profiles",
                   data = "galah:::check_internal_cache()$show_all_profiles")
  }
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` providers
#' @noRd
#' @keywords Internal
collapse_providers <- function(){
  result <- list(type = "metadata/providers",
                 url = url_lookup(method = "metadata",
                                  type = "providers"),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` reasons
#' @noRd
#' @keywords Internal
collapse_reasons <- function(){
  update_needed <- internal_cache_update_needed("show_all_reasons")
  if(update_needed){
    result <- list(type = "metadata/reasons",
                   url = url_lookup(method = "metadata",
                                    type = "reasons"),
                   headers = build_headers())    
  }else{
    result <- list(type = "metadata/reasons",
                   data = "galah:::check_internal_cache()$show_all_reasons")
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

#' Internal function to `collapse()` identifiers
#' @noRd
#' @keywords Internal
collapse_identifiers <- function(.data){
  if(is.null(.data$identify)){
    url_list <- url_lookup("names_lookup")
    names(url_list) <- "no-name-supplied"
  }else{
    base_url <- url_lookup(method = "metadata",
                           type = "identifiers") |>
                url_parse()
    
    # split if there are multiple identifiers
    if(length(.data$identify) > 1) {
      # multiple
      query <- split(.data$identify, seq_along(.data$identify))
    } else {
      # single
      query <- list(.data$identify)
    }
    
    # create query urls
    urls <- lapply(query,
                       function(a, base_url){
                         names(a) <- "taxonID"
                         base_url$query <- as.list(a)
                         url_build(base_url)
                       },
                       base_url = base_url) |>
      unlist()
    search_terms <- .data$identify
  }
  
  # build object and return
  result <- list(type = .data$type,
                 url = tibble(url = urls, 
                              search_term = search_terms),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}
