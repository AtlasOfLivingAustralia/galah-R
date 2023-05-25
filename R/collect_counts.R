#' Collect counts
#' @noRd
#' @keywords Internal
collect_counts <- function(.data){
  if(is.null(.data$groups)){
    result <- url_GET(.data$url, .data$query)
    result[[.data$column]]
  }else{
    if(is_gbif()){
      collect_grouped_counts_GBIF(.data)
    }else{
      collect_grouped_counts_LA(.data)
    }
  }
}

#' Collect grouped counts for LAs
#' @noRd
#' @keywords Internal
collect_grouped_counts_LA <- function(.data){
  
  if(.data$groups$expand){
    browser()
    all_queries <- build_query_list(.data)
    result <- lapply(all_queries, run_grouped_count_LA)
  }else{
    run_grouped_count_LA(.data) |>
    bind_rows()
  }
}

#' Simple function to run a facet query for LAs
#' #' @noRd
#' @keywords Internal
run_grouped_count_LA <- function(.data){
  result <- url_GET(.data$url, .data$query)
  result[[.data$column]]
}

#' Determine set of queries when expand = TRUE
#' @noRd
#' @param x list returned from `run_grouped_count_LA()`
#' @keywords Internal
build_query_list <- function(.data, x){
  
  result <- run_grouped_count_LA(.data)
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
  levels_list <- split(levels_df, seq_len(nrow(levels_df)))
  
  filter_list <- lapply(levels_list, function(a){
    c(list(
      fq = c(.data$query$fq, a[[1]]), # messy
        # fw needs to be concatenated here; some code in utilities and other code in atlas_counts
      facets = facets_large 
    ), .data$query[!names(.data$query) %in% c("facets", "fq")])
  })

  query_list <- lapply(filter_list, function(a){
    list(url = .data$url, query = a)
  })
  
  return(query_list)

}
