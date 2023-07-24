#' Compute counts
#' 
#' This is a little different from other `compute` functions; it converts a 
#' `data_query` object (from `collapse(type = "occurrences-count")`) into one or more urls
#' that can be passed verbatim to `query_API()`. This is important when `group_by`
#' has `expand = TRUE`, because it requires an initial call to the atlas to 
#' calculate the requisite URLs. It makes very little difference under other
#' circumstances
#' @param .data An object of class `data_query`
#' @keywords Internal
#' @noRd
compute_counts <- function(.data){
  if(.data$expand){
    if(is_gbif()){
      abort("Grouped counts haven't been (re)implemented for GBIF yet")
      # compute_grouped_counts_GBIF(.data)
    }else{
      result <- list(
        type = .data$type,
        url = build_query_list_LA(.data),
        slot_name = .data$slot_name)
    }    
  }else{
    result <- .data
  }
  class(result) <- "data_response"
  attr(result, "fields") <- .data$query$facets
  return(result)
}

#' Determine set of queries when expand = TRUE
#' @noRd
#' @param x list returned from `run_grouped_count_LA()`
#' @keywords Internal
build_query_list_LA <- function(.data){
  
  result <- url_GET(.data$url, .data$query)[[.data$column]]
  
  if(is.null(result)){
    system_down_message("count")
  }
  
  values_df <- data.frame(
    facet = unlist(.data$query[names(.data$query) == "facets"]),
    n = unlist(lapply(result, nrow)))
  
  names(result) <- values_df$facet
  
  # work out which to pass as facets vs those we iterate over with lapply
  facets_large <- values_df$facet[which.max(values_df$n)]
  facets_small <- values_df$facet[values_df$facet != facets_large]
  
  # convert to list of valid queries
  levels_df <- expand.grid(
    lapply(result[facets_small], function(a){a$fq}),
    stringsAsFactors = FALSE)
  attr(levels_df, "out.attrs") <- NULL
  levels_list <- split(levels_df, seq_len(nrow(levels_df)))
  
  # create corresponding df of labels, for matching to counts returned later
  labels_df <- expand.grid(
    lapply(result[facets_small], function(a){a$label}),
    stringsAsFactors = FALSE)
  attr(levels_df, "out.attrs") <- NULL

  # build a set of valid queries
  query_list <- lapply(levels_list, function(a, query){
    all_queries <- c(query$fq,
                     glue_collapse(do.call(c, a), " AND "))
    list(
      fq = glue_collapse(all_queries, " AND "),
      facets = facets_large) |>
    c(query[!(names(query) %in% c("facets", "fq"))])
  }, query = .data$query)
  
  # convert queries to urls
  url_list <- lapply(query_list, function(a, url){
    build_url_internal(list(url = url, query = a))
  }, url = .data$url)
  
  # combine into a tibble
  result_df <- tibble(
    labels_df,
    url = as.character(url_list))
  
  # preserve name of url field
  attr(result_df, "url_field") <- facets_large
  
  return(result_df)
  
}
