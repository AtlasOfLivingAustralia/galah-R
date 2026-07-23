#' Internal function to run `capture()` for 
#' `request_metadata(type = "fields") |> unnest()`
#' @noRd
#' @keywords Internal
capture_fields_unnest <- function(.query){
  .query$type <- "metadata/fields-unnest"
  url <- url_lookup(.query) |> 
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
capture_lists_unnest <- function(.query){
  .query$type <- "metadata/lists-unnest"
  # get list lookup url
  url <- url_lookup(.query,
                    list_id = .query$filter$value[1]) |>
    httr2::url_parse()
  browser()
  # set a default query
  url$query <- list(
    pageSize = 30000,      # 30000 is the max, a few lists have >30000 items
    includeKVP = TRUE      # add name & status columns
    )  
  
  # create object
  x <- list(type = .query$type,
       atlas = .query$atlas,
       url = httr2::url_build(url))
  
  # return lists longer than max pageSize
  lists_gt_max_page <- retrieve_cache("lists") |>
    dplyr::filter(row_count > 30000) |>
    dplyr::pull(species_list_uid)
  
  # if list is long, add more queries to match required page number for list row_count
  if(.query$filter$value %in% lists_gt_max_page) {
    # browser()
    row_count <- retrieve_cache("lists") |>
      dplyr::filter(species_list_uid == .query$filter$value) |>
      dplyr::pull(row_count)
    n_pages <- ceiling(row_count * (1/30000)) # (i.e. 1/1000)
    
    # add additional urls to reach required number of pages to return all items
    x$url <- tibble::tibble(url = glue::glue("{x$url}&page={seq_len(n_pages)}"))
  }
  # browser()
  # create query
  x |> as_query()
  
  # I thought this would work but it doesn't
  
  # create object
  # list(type = .query$type,
  #      atlas = .query$atlas,
  #      url = httr2::url_build(url))  |>
  #   as_query()
}

#' Internal function to run `capture()` for 
#' `request_metadata(type = "profiles") |> unnest()`
#' @noRd
#' @keywords Internal
capture_profiles_unnest <- function(.query){
  .query$type <- "metadata/profiles-unnest"
  list(type = .query$type,
       atlas = .query$atlas,
       url = url_lookup(.query, 
                        profile = .query$filter$value[1]))  |>
    as_prequery()
}

#' Internal function to `capture()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @noRd
#' @keywords Internal
capture_taxa_unnest <- function(.query){
  .query$type <- "metadata/taxa-unnest"
  if(!is.null(.query$filter)){
    id <- .query$filter$value[1]
  }else if(!is.null(.query$identify)){
    id <- "`TAXON_PLACEHOLDER`"
  }
  list(type = .query$type,
       atlas = .query$atlas,
       url = url_lookup(.query, id = id),
       headers = build_headers()) |>
    as_prequery()
}
