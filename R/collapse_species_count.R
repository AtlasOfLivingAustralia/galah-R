#' Internal function to compute for species counts
#' @noRd
#' @keywords Internal
collapse_species_count <- function(.query){
  browser()
  # `expand` argument has been removed from query objects; need to refactor this
  if(.query$expand){
    .query <- collapse_species_query_list(.query)
  }else{
    .query$url <- tibble::tibble(url = .query$url)
  }
  .query
}

#' Internal function to generate correct set of species-count queries when 
#' `group_by()` is set
#' @noRd
#' @keywords Internal 
collapse_species_query_list <- function(.query){
  
  # remove `species_facets()` from query
  url <- httr2::url_parse(.query$url)
  query_temp <- url$query
  query_temp <- query_temp[-which(
    unlist(query_temp) == species_facets() & 
    names(query_temp) == "facets")]
  n_facet_terms <- length(which(names(query_temp) == "facets"))
  url$query <- c(query_temp, list(pageSize = 0))
  
  # rebuild a .query object for this query
  data_temp <- .query
  data_temp$type <- "data/occurrences-count"
  data_temp$url <- httr2::url_build(url)
  
  # collect using `occurrences-count` code (to parse expand correctly)
  df <- collect(data_temp)
  
  # create new set of fq args 
  fq_args <- purrr::map(
    split(df, seq_len(nrow(df))),
    \(a){
      x <- a[, - ncol(a)]
      glue::glue_collapse(
        glue::glue("{names(x)}:{x}"),
        sep = " AND ")
    }) |> unlist()
  
  # modify url to only have `species_facets()` in facets slot
  url <- httr2::url_parse(.query$url)
  query <- url$query
  query <- query[-which(
    unlist(query_temp) != species_facets() & 
    names(query_temp) == "facets")]
  query$flimit <- 1
  url$query <- query
  
  # create new fq urls
  new_fqs <- paste(url$query$fq, fq_args, sep = " AND ")
  urls <- purrr::map(new_fqs, function(a, x){
    x$query$fq <- a
    httr2::url_build(x)
  }, x = url) |> 
    unlist()
  
  # convert to a tibble to pass back to .query
  .query$url <- dplyr::bind_cols(
    dplyr::select(df, -count),
    tibble::tibble(url = urls))
  return(.query)
}
