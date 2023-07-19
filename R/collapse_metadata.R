#' Internal function to `collapse()` apis
#' @noRd
#' @keywords Internal
collapse_apis <- function(.data){
  result <- list(type = .data$type,
                 data = "galah:::node_config")
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` assertions
#' NOTE: API doesn't accept any arguments - could post-filter for search
#' @noRd
#' @keywords Internal
collapse_assertions <- function(.data){
  if(is_gbif()){
    result <- list(type = .data$type,
                   data = "galah:::gbif_internal_archived$assertions")
  }else{
    result <- list(type = .data$type,
                   url = url_lookup("records_assertions"),
                   headers = build_headers())
  }
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` atlases
#' @noRd
#' @keywords Internal
collapse_atlases <- function(.data){
  result <- list(type = .data$type,
                 data = "galah:::node_metadata")
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` collections
#' @noRd
#' @keywords Internal
collapse_collections <- function(.data){
  result <- list(type = .data$type,
                 url = url_lookup("collections_collections"),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` datasets
#' @noRd
#' @keywords Internal
collapse_collections <- function(.data){
  result <- list(type = .data$type,
                 url = url_lookup("collections_datasets"),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` fields
#' Note that this is currently written to be consistent with past versions of
#' `show_all_fields()`. It is somewhat inconsistent with the rest of the package
#' to have a single function call so many different data sources.
#' An alternative would be to have a function that called 
#' `url_lookup("records_fields")` and have show_all_fields call that + collect("layers")
#' @noRd
#' @keywords Internal
collapse_fields <- function(.data){
  result <- list(type = .data$type,
                 url = url_lookup("records_fields"),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result) 
}

#' Internal function to `collapse()` layers
#' @noRd
#' @keywords Internal
collapse_layers <- function(.data){
  result <- list(type = .data$type,
                 url = url_lookup("spatial_layers"),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result) 
}

#' Internal function to `collapse()` licences
#' @noRd
#' @keywords Internal
collapse_licences <- function(.data){
  result <- list(type = .data$type,
                 url = url_lookup("image_licences"),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result) 
}

#' Internal function to `collapse()` lists
#' Note: likely to require pagination, and therefore a `compute()` stage
#' to calculate how many urls are needed
#' @noRd
#' @keywords Internal
collapse_lists <- function(.data){
  result <- list(type = .data$type,
                 url = url_lookup("lists_all"),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result) 
}

#' Internal function to `collapse()` profiles
#' @noRd
#' @keywords Internal
collapse_profiles <- function(.data){
  update_needed <- TRUE # internal_cache_update_needed("show_all_profiles")
  if(update_needed){
    result <- list(type = .data$type,
                   url = url_lookup("profiles_all"),
                   headers = build_headers())    
  }else{
    result <- list(type = .data$type,
                   data = "galah:::check_internal_cache()$show_all_profiles")
  }
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` providers
#' @noRd
#' @keywords Internal
collapse_providers <- function(.data){
  result <- list(type = .data$type,
                 url = url_lookup("collections_providers"),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` reasons
#' @noRd
#' @keywords Internal
collapse_reasons <- function(.data){
  update_needed <- TRUE # internal_cache_update_needed("show_all_reasons")
  if(update_needed){
    result <- list(type = .data$type,
                   url = url_lookup("logger_reasons"),
                   headers = build_headers())    
  }else{
    result <- list(type = .data$type,
                   data = "galah:::check_internal_cache()$show_all_reasons")
  }
  class(result) <- "metadata_query"
  return(result)
}

#' Internal function to `collapse()` ranks
#' @noRd
#' @keywords Internal
collapse_ranks <- function(.data){
  if(is_gbif()){
    result <- list(type = .data$type,
                   data = "galah:::gbif_internal_archived$ranks")
  }else{
    result <- list(type = .data$type,
                   data = "galah:::galah_internal_archived$ranks")
  }
  class(result) <- "metadata_query"
  return(result)
}