#' Internal function to compute for species counts
#' @importFrom tibble as_tibble
#' @noRd
#' @keywords Internal
compute_species_count <- function(.query){
  if(.query$expand){
    .query <- build_species_query_list(.query)
  }else{
    .query$url <- as_tibble(data.frame(url = .query$url))
  }
  .query
}

#' Internal function to generate correct set of species-count queries when 
#' `group_by()` is set
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal 
build_species_query_list <- function(.query){
  
  # remove `species_facets()` from query
  url <- url_parse(.query$url)
  query_temp <- url$query
  query_temp <- query_temp[-which(
    unlist(query_temp) == species_facets() & 
    names(query_temp) == "facets")]
  n_facet_terms <- length(which(names(query_temp) == "facets"))
  url$query <- c(query_temp, list(pageSize = 0))
  
  # rebuild a .query object for this query
  data_temp <- .query
  data_temp$type <- "data/occurrences-count"
  data_temp$url <- url_build(url)
  data_temp$expand <- ifelse(n_facet_terms > 1, TRUE, FALSE)
  
  # collect using `occurrences-count` code (to parse expand correctly)
  df <- collect(data_temp)
  
  # create new set of fq args 
  fq_args <- lapply(
    split(df, seq_len(nrow(df))),
    function(a){
      x <- a[, - ncol(a)]
      paste(
        paste(names(x), x, sep = ":"),
        collapse = " AND ")
    }) |> unlist()
  
  # modify url to only have `species_facets()` in facets slot
  url <- url_parse(.query$url)
  query <- url$query
  query <- query[-which(
    unlist(query_temp) != species_facets() & 
    names(query_temp) == "facets")]
  query$flimit <- 1
  url$query <- query
  
  # create new fq urls
  new_fqs <- paste(url$query$fq, fq_args, sep = " AND ")
  urls <- lapply(new_fqs, function(a, x){
    x$query$fq <- a
    url_build(x)
  }, x = url) |> unlist()
  
  # convert to a tibble to pass back to .query
  .query$url <- bind_cols(
    select(df, -count),
    tibble(url = urls))
  return(.query)
}
