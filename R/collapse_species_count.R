#' Internal function to compute for species counts
#' @noRd
#' @keywords Internal
collapse_species_count <- function(.query){
  if(!is.null(.query$request$group_by)){
    collapse_species_query_list(.query)
  }else{
    .query$url <- tibble::tibble(url = .query$url)
    .query
  }
}

#' Internal function to generate correct set of species-count queries when 
#' `group_by()` is set
#' @noRd
#' @keywords Internal 
collapse_species_query_list <- function(.query){
  # use existing `group_by()` code to handle the facet construction
  new_query <- collapse_occurrences_count_atlas_groupby_crossed(.query)
  distinct_facet <- .query$request$distinct$name # this is not retained for some reason
  new_urls <- new_query |>
    purrr::pluck("url", "url") |>
    purrr::map(.f = \(a){
      url_now <- httr2::url_parse(a)
      url_now$query$flimit <- 0 # set no facets to be returned; we only want the number of facets
      url_now$query$facets <- distinct_facet # set facet correctly
      httr2::url_build(url_now)
    }) |>
    unlist()
  new_query$url$url <- new_urls
  new_query
}