# These functions are called by `capture.metadata_request()`

# Utility functions to build default forms of query

#' Internal function to define a standard query
#' @param query_type the sting used to define that query type; see `show_all_apis()`
#' @noRd
#' @keywords Internal
default_query <- function(x){
  list(type = x$type,
       atlas = x$atlas,
       url = url_lookup(x),
       headers = build_headers())
}

#' Ditto for cache
#' @noRd
#' @keywords Internal
default_cache <- function(x){
  specific_type <- stringr::str_remove(x$type, "^metadata/")
  list(type = x$type,
       atlas = x$atlas,
       data = glue::glue("galah:::retrieve_cache(\"{specific_type}\")")) 
}

#' Modified version of `default_query()` that supports filtering
#' @noRd
#' @keywords Internal
filtered_query <- function(x){
  url <- url_lookup(x) |>
    httr2::url_parse()
  url$query <- list(q = x$filter$value[1])
  list(type = x$type,
       atlas = x$atlas,
       url = httr2::url_build(url),
       headers = build_headers()) 
}

# Actual functions called to build those queries

#' Internal function get a tibble of APIs
#' @noRd
#' @keywords Internal
capture_apis <- function(x){
  list(type = "metadata/apis",
       atlas = x$atlas,
       data = "galah:::node_config") |>
    as_query()
}

#' Internal function to create an assertions query
#' NOTE: API doesn't accept any arguments - could post-filter for search
#' @noRd
#' @keywords Internal
capture_assertions <- function(x){
  x$type <- "metadata/assertions"
  if(x$atlas == "Global"){
    result <- list(type = x$type,
                   atlas = x$atlas,
                   data = "galah:::gbif_internal_archived$assertions")
  }else{
    if(check_if_cache_update_needed(x, "assertions")){
      result <- default_query(x)
    }else{
      result <- default_cache(x)
    }
  }
  result |>
    as_query()
}

#' Internal function to create an atlases query
#' @noRd
#' @keywords Internal
capture_atlases <- function(x){
  x$type <- "metadata/atlases"
  list(type = x$type,
       atlas = x$atlas,
       data = "galah:::node_metadata") |>
    as_query()
}

#' Internal function to create a collections query
#' @noRd
#' @keywords Internal
capture_collections <- function(x){
  # set `type`
  x$type <- "metadata/collections"
  # If `filter()` is supplied, we always need a query
  if(x$atlas == "Global" & !missing(x)){
    if(!is.null(x$filter)){
      result <- filtered_query(x)
    }else{
      result <- default_query(x)
    }
  # If no `filter()`, check cache instead
  }else{
    if(check_if_cache_update_needed(x, "collections")){
      result <- default_query(x)
    }else{
      result <- default_cache(x)
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
capture_config <- function(x){
  x$type <- "metadata/config"
  if(check_if_cache_update_needed(x, "config")){
    result <- default_query(x)
  }else{
    result <- default_cache(x)     
  }
  result |>
    as_query()
}

#' Internal function to create a datasets query
#' @noRd
#' @keywords Internal
capture_datasets <- function(x){
  # set `type`
  x$type <- "metadata/datasets"
  # If `filter()` is supplied, we always need a query
  if(x$atlas == "Global" & !missing(x)){
    if(!is.null(x$filter)){
      result <- filtered_query(x)
    }else{
      result <- default_query(x)
    }
    # If no `filter()`, check cache instead
  }else{
    if(check_if_cache_update_needed(x, "datasets")){
      result <- default_query(x)
    }else{
      result <- default_cache(x)
    }
  }
  result |>
    as_query()
}

#' Internal function to create a fields query
#' @noRd
#' @keywords Internal
capture_fields <- function(x){
  x$type <- "metadata/fields"
  if(check_if_cache_update_needed(x, "fields")){
    default_query(x) |> as_query()
  }else{
    default_cache(x) |> as_query()  
  }
}

#' Internal function to create a licences query
#' @noRd
#' @keywords Internal
capture_licences <- function(x){
  x$type <- "metadata/licences"
  if(check_if_cache_update_needed(x, "licences")){
    default_query(x) |> as_query()
  }else{
    default_cache(x) |> as_query()  
  }
}

#' Internal function to create a lists query
#' @noRd
#' @keywords Internal
capture_lists <- function(x,
                          error_call = rlang::caller_env()){
  x$type <- "metadata/lists"
  # if filter is supplied, lookup a specified list by dr number
  if(!is.null(x$filter)){
    dr_lookup <- stringr::str_detect(x$filter$value, "^dr")
    if(any(dr_lookup)){
      dr_values <- x$filter$value[dr_lookup]
      base_url <- url_lookup(x)
      url <- glue::glue("{base_url}/{dr_values}")
      if(length(url) > 1){
        result <- list(type = x$type,
                       atlas = x$atlas,
                       url = tibble::tibble(url = url), # note: tibbles are used to skip pagination in `collapse()`
                       headers = build_headers())
      }else{
        result <- list(type = x$type,
                       atlas = x$atlas,
                       url = url,
                       headers = build_headers())
      }
    }else{
      cli::cli_abort(c("`filter()` arguments to `lists` only accept a data resource number",
                       i = "e.g. request_metadata() |> filter(lists == 'dr656')"),
                     call = error_call)
    }
  # if filter isn't supplied, check cache etc
  }else{
    if(check_if_cache_update_needed(x, "lists")){
      url <- url_lookup(x)
      if(!missing(x)){
        url <- url |> httr2::url_parse()
        # note: page size is set in `collapse()` rather than here
        # as it allows more sensible assessment of pagination requirements
        url$query <- switch(x$atlas, 
                "Australia" = {list(pageSize = 1)},
                list(max = 10000))
        url <- httr2::url_build(url)
      }
      result <- list(type = x$type,
                     atlas = x$atlas,
                     url = url,
                     headers = build_headers())
    }else{
      result <- default_cache(x)
    } 
  }
  result |>
    as_prequery()
}

#' Internal version of `capture()` for `request_metadata(type = "media")`
#' @param .query An object of class `metadata_request` (from `request_metadata()`)
#' @noRd
#' @keywords Internal
capture_media_metadata <- function(.query,
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
  .query$type <- "metadata/media"
  list(type = .query$type,
       atlas = .query$atlas,
       url = tibble::tibble(url = url_lookup(.query,
                                             id = .query$filter$value)),
       headers = build_headers()) |>
    as_query()
  
}

#' Internal function to create a profiles query
#' @noRd
#' @keywords Internal
capture_profiles <- function(x){
  x$type <- "metadata/profiles"
  if(check_if_cache_update_needed(x, "profiles")){
    result <- default_query(x)
  }else{
    result <- default_cache(x)     
  }
  result |>
    as_query()
}

#' Internal function to create a providers query
#' @noRd
#' @keywords Internal
capture_providers <- function(x){
  # set `type`
  x$type <- "metadata/providers"
  # If `filter()` is supplied, we always need a query
  if(x$atlas == "Global" & !missing(x)){
    if(!is.null(x$filter)){
      result <- filtered_query(x)
    }else{
      result <- default_query(x)
    }
    # If no `filter()`, check cache instead
  }else{
    if(check_if_cache_update_needed(x, "providers")){
      result <- default_query(x)
    }else{
      result <- default_cache(x)
    }
  }
  result |>
    as_query()
}

#' Internal function to create a reasons query
#' @noRd
#' @keywords Internal
capture_reasons <- function(x){
  x$type <- "metadata/reasons"
  if(check_if_cache_update_needed(x, "reasons")){
    result <- default_query(x)
  }else{
    result <- default_cache(x)     
  }
  result |>
    as_query()
}

#' Internal function to create a ranks query
#' @noRd
#' @keywords Internal
capture_ranks <- function(x){
  list(type = "metadata/ranks",
       atlas = x$atlas,
       data = ifelse(x$atlas == "Global",
                     "galah:::gbif_internal_archived$ranks",
                     "galah:::galah_internal_archived$ranks")) |>
    as_query()
}