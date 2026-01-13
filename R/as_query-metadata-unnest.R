#' Internal function to run `as_query()` for 
#' `request_metadata(type = "fields") |> unnest()`
#' @noRd
#' @keywords Internal
as_query_fields_unnest <- function(.query){
  url <- url_lookup("metadata/fields-unnest") |> 
    httr2::url_parse()
  if(is_gbif()){
    url$query <- list(limit = 0,
                      facet = .query$filter$value[1], # note: facet (singular), not facets (plural)
                      facetLimit = 10^4)    
  }else{
    url$query <- list(facets = .query$filter$value[1],
                      flimit = 10^4)
  }
  list(type = "metadata/fields-unnest",
       url = httr2::url_build(url)) |>
    as_prequery()
}

#' Internal function to run `as_query()` for 
#' `request_metadata(type = "lists") |> unnest()`
#' @noRd
#' @keywords Internal
as_query_lists_unnest <- function(.query){
  # get list lookup url
  url <- url_lookup("metadata/lists-unnest",
                    list_id = .query$filter$value[1]) |>
    httr2::url_parse()
  # set a default query
  url$query <-  list(max = -1,           # remove max limit
                     includeKVP = TRUE)  # add name & status columns
  # create object
  list(type = "metadata/lists-unnest",
       url = httr2::url_build(url))  |>
    as_query()
}

#' Internal function to run `as_query()` for 
#' `request_metadata(type = "profiles") |> unnest()`
#' @noRd
#' @keywords Internal
as_query_profiles_unnest <- function(.query){
  list(type = "metadata/profiles-unnest",
       url = url_lookup("metadata/profiles-unnest", 
                        profile = .query$filter$value[1]))  |>
    as_prequery()
}

#' Internal function to `as_query()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @noRd
#' @keywords Internal
as_query_taxa_unnest <- function(.query){
  if(!is.null(.query$filter)){
    id <- .query$filter$value[1]
  }else if(!is.null(.query$identify)){
    id <- "`TAXON_PLACEHOLDER`"
  }
  list(type = "metadata/taxa-unnest",
       url = url_lookup("metadata/taxa-unnest", id = id),
       headers = build_headers()) |>
    as_prequery()
}
