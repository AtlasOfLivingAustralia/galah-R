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
as_query_apis <- function(x){
  list(type = "metadata/apis",
       data = "galah:::node_config") |>
    as_query()
}

#' Internal function to create an assertions query
#' NOTE: API doesn't accept any arguments - could post-filter for search
#' @noRd
#' @keywords Internal
as_query_assertions <- function(x){
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
  result |>
    as_query()
}

#' Internal function to create an atlases query
#' @noRd
#' @keywords Internal
as_query_atlases <- function(x){
  list(type = "metadata/atlases",
       data = "galah:::node_metadata") |>
    as_query()
}

#' Internal function to create a collections query
#' @noRd
#' @keywords Internal
as_query_collections <- function(x){
  # set `type`
  query_type <- "metadata/collections"
  # If `filter()` is supplied, we always need a query
  if(is_gbif() & !missing(x)){
    if(!is.null(x$filter)){
      result <- filtered_query(query_type, x)
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
  result |>
    as_query()
}
# NOTE: LA collectory functions do not accept `max` or `offset`
# Therefore they cannot be paginated. GBIF collectory funs can.

#' Internal function to create an auth-config query
#' @noRd
#' @keywords Internal
as_query_config <- function(x){
  query_type <- "metadata/config"
  if(check_if_cache_update_needed("config")){
    result <- default_query(query_type)
  }else{
    result <- default_cache(query_type)     
  }
  result |>
    as_query()
}

#' Internal function to create a datasets query
#' @noRd
#' @keywords Internal
as_query_datasets <- function(x){
  # set `type`
  query_type <- "metadata/datasets"
  # If `filter()` is supplied, we always need a query
  if(is_gbif() & !missing(x)){
    if(!is.null(x$filter)){
      result <- filtered_query(query_type, x)
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
  result |>
    as_query()
}

#' Internal function to create a fields query
#' @noRd
#' @keywords Internal
as_query_fields <- function(x){
  query_type <- "metadata/fields"
  if(check_if_cache_update_needed("fields")){
    default_query(query_type) |> as_query()
  }else{
    default_cache(query_type) |> as_query()  
  }
}

#' Internal function to create a licences query
#' @noRd
#' @keywords Internal
as_query_licences <- function(x){
  query_type <- "metadata/licences"
  if(check_if_cache_update_needed("licences")){
    default_query(query_type) |> as_query()
  }else{
    default_cache(query_type) |> as_query()  
  }
}

#' Internal function to create a lists query
#' @noRd
#' @keywords Internal
as_query_lists <- function(x,
                           error_call = rlang::caller_env()){
  query_type <- "metadata/lists"
  # if filter is supplied, lookup a specified list by dr number
  if(!is.null(x$filter)){
    dr_lookup <- stringr::str_detect(x$filter$value, "^dr")
    if(any(dr_lookup)){
      dr_values <- x$filter$value[dr_lookup]
      base_url <- url_lookup(query_type)
      url <- glue::glue("{base_url}/{dr_values}")
      result <- list(type = query_type,
                     url = tibble::tibble(url = url), # note: tibbles are used to skip pagination in `collapse()`
                     headers = build_headers())
    }else{
      cli::cli_abort(c("`filter()` arguments to `lists` only accept a data resource number",
                       i = "e.g. request_metadata() |> filter(lists == 'dr656')"),
                     call = error_call)
    }
  # if filter isn't supplied, check cache etc
  }else{
    if(check_if_cache_update_needed("lists")){
      url <- url_lookup(query_type) |>
        httr2::url_parse()
      url$query <- list(max = 10000)
      if(!missing(x)){
        if(!is.null(x$slice)){
          url$query <- list(max = x$slice$slice_n)
        }    
      }
      result <- list(type = query_type,
                     url = httr2::url_build(url),
                     headers = build_headers())
    }else{
      result <- default_cache(query_type)
    } 
  }
  result |>
    as_query()
}

#' Internal version of `as_query()` for `request_metadata(type = "media")`
#' @param .query An object of class `metadata_request` (from `request_metadata()`)
#' @noRd
#' @keywords Internal
as_query_media_metadata <- function(.query,
                                    error_call = rlang::caller_env()){
  # NOTE:
  # this function currently assumes that the user has passed an occurrence 
  # tibble verbatim to filter, i.e.
  # `request_metadata() |> filter(media = occurrences) |> collapse()`
  # It may be useful to support passing of media_ids directly, e.g.
  # `request_metadata() |> filter(media = occurrences$images`) |> collapse()
  if(is.null(.query$filter)){
    cli::cli_abort("Requests for metadata of type = \"media\" must have information passed via `filter()`",
                   call = error_call)
  }
  
  ## Move this to `atlas_media()`
  # occ <- .query$filter$data
  # if(any(colnames(occ) %in% c("images", "videos", "sounds"))){ # Australia, Sweden, Spain
  #   media_cols <- which(colnames(occ) %in% c("images", "videos", "sounds"))
  #   media_ids <- do.call(c, occ[, media_cols]) |>
  #     unlist()
  #   media_ids <- media_ids[!is.na(media_ids)]
  #   names(media_ids) <- NULL
  # }else if(any(colnames(occ) == "all_image_url")){ # Austria, Sweden, UK
  #   media_ids <- dplyr::pull(occ, "all_image_url")
  #   media_ids <- media_ids[!is.na(media_ids)]
  #   names(media_ids) <- NULL
  # }else{
  #   cli::cli_abort("Media metadata not found in supplied tibble",
  #                  call = error_call)
  # }
  list(type = "metadata/media",
       url = tibble::tibble(url = url_lookup("metadata/media",
                                             id= .query$filter$value)),
       headers = build_headers(),
       filter = .query$filter) |>
    as_query()
  
}

#' Internal function to create a profiles query
#' @noRd
#' @keywords Internal
as_query_profiles <- function(x){
  query_type <- "metadata/profiles"
  if(check_if_cache_update_needed("profiles")){
    result <- default_query(query_type)
  }else{
    result <- default_cache(query_type)     
  }
  result |>
    as_query()
}

#' Internal function to create a providers query
#' @noRd
#' @keywords Internal
as_query_providers <- function(x){
  # set `type`
  query_type <- "metadata/providers"
  # If `filter()` is supplied, we always need a query
  if(is_gbif() & !missing(x)){
    if(!is.null(x$filter)){
      result <- filtered_query(query_type, x)
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
  result |>
    as_query()
}

#' Internal function to create a reasons query
#' @noRd
#' @keywords Internal
as_query_reasons <- function(x){
  query_type <- "metadata/reasons"
  if(check_if_cache_update_needed("reasons")){
    result <- default_query(query_type)
  }else{
    result <- default_cache(query_type)     
  }
  result |>
    as_query()
}

#' Internal function to create a ranks query
#' @noRd
#' @keywords Internal
as_query_ranks <- function(x){
  if(is_gbif()){
    result <- list(type = "metadata/ranks",
                   data = "galah:::gbif_internal_archived$ranks")
  }else{
    result <- list(type = "metadata/ranks",
                   data = "galah:::galah_internal_archived$ranks")
  }
  result |>
    as_query()
}