#' Internal function to run `capture()` for 
#' `request_metadata(type = "fields") |> unnest()`
#' @noRd
#' @keywords Internal
capture_fields_unnest <- function(.query, call = rlang::caller_env()){
  .query$type <- "metadata/fields-unnest"
  url <- url_lookup(.query, error_call = call) |> 
    httr2::url_parse()
  if(.query$atlas == "Global"){
    url$query <- list(limit = 0,
                      facet = .query$filter$value[1], # note: facet (singular), not facets (plural)
                      facetLimit = 10^4)    
  }else{
    url$query <- list(facets = .query$filter$value[1],
                      flimit = 10^4)
  }
  list(type = .query$type,
       atlas = .query$atlas,
       url = httr2::url_build(url)) |>
    as_prequery()
}

#' Internal function to run `capture()` for 
#' `request_metadata(type = "lists") |> unnest()`
#' @noRd
#' @keywords Internal
capture_lists_unnest <- function(.query, call = rlang::caller_env()){
  .query$type <- "metadata/lists-unnest"
  # get list lookup url
  url <- url_lookup(.query,
                    list_id = .query$filter$value[1], 
                    error_call = call) |>
    httr2::url_parse()
  
  # set a default query
  url$query <- list(
    pageSize = 30000,      # 30000 is the max, a few lists have >30000 items
    includeKVP = TRUE      # add name & status columns
    )  
  
  # create object
  list(type = .query$type,
       atlas = .query$atlas,
       url = httr2::url_build(url)) |>
    as_prequery()
  # note: class prequery is necessary to ensure checking of list length
}

#' Internal function to run `capture()` for 
#' `request_metadata(type = "profiles") |> unnest()`
#' @noRd
#' @keywords Internal
capture_profiles_unnest <- function(.query, call = rlang::caller_env()){
  .query$type <- "metadata/profiles-unnest"
  list(type = .query$type,
       atlas = .query$atlas,
       url = url_lookup(.query, 
                        profile = .query$filter$value[1], 
                        error_call = call))  |>
    as_prequery()
}

#' Internal function to `capture()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @noRd
#' @keywords Internal
capture_taxa_unnest <- function(.query, call = rlang::caller_env()){
  .query$type <- "metadata/taxa-unnest"
  if(!is.null(.query$filter)){
    id <- .query$filter$value[1]
  }else if(!is.null(.query$identify)){
    id <- "`TAXON_PLACEHOLDER`"
  }
  
  # set url depending on org
  if(.query$atlas == "Global"){
    url <- url_lookup(.query,
                      dataset = gbif_checklistbank_key(),
                      id = id,
                      error_call = call)
  }else{
    url <- url_lookup(.query,
                      id = id,
                      error_call = call)
  }

  # return prequery object
  list(type = .query$type,
       atlas = .query$atlas,
       url = url,
       headers = build_headers()) |>
    as_prequery()
}
