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
compute_occurrences_count <- function(.data){
  if(.data$expand){
    if(is_gbif()){
      abort("Grouped counts haven't been (re)implemented for GBIF yet")
      # compute_grouped_counts_GBIF(.data)
    }else{
      result <- c(list(
        type = .data$type,
        url = build_query_list_LA(.data)),
        .data[!(names(.data) %in% c("url", "type"))])
    }    
  }else{
    result <- adjust_flimit(.data)
  }
  class(result) <- "data_response"
  return(result)
}

#' Internal function to handle facet counting, adjustment etc.
#' @noRd
#' @keywords Internal
adjust_flimit <- function(.data){
  url <- url_parse(.data$url)
  if(!is.null(url$query$flimit)){
    if(as.integer(url$query$flimit) < 1){
      n_facets <- check_facet_count(.data)
      url$query$flimit <- .data$arrange$slice_n # Q: is this correct? 
      if(.data$arrange$slice_n < n_facets){
        url$query$foffset <- n_facets - .data$arrange$slice_n
      }
      .data$url <- url_build(url) 
      .data
    }else{
      .data
    }
  }else{
    .data
  }
}

#' Determine set of queries when expand = TRUE
#' @importFrom dplyr bind_rows
#' @importFrom dplyr c_across
#' @importFrom dplyr full_join 
#' @importFrom dplyr rowwise
#' @importFrom dplyr starts_with
#' @importFrom dplyr ungroup
#' @importFrom glue glue_collapse
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @noRd
#' @param x list returned from `run_grouped_count_LA()`
#' @keywords Internal
build_query_list_LA <- function(.data){
  # get url
  url <- url_parse(.data$url)
  
  # remove last-provided facet
  facet_list <- url$query[names(url$query) == "facets"]
  facet_names <- unlist(facet_list)
  names(facet_names) <- NULL

  # save out facet limits to add back later
  saved_facet_queries <- list( 
    flimit = url$query$flimit,
    foffset = url$query$foffset)
  saved_facet_queries <- saved_facet_queries[
    unlist(lapply(saved_facet_queries, function(a){!is.null(a)}))]
  
  # rebuild url
  url$query <- c(
    url$query[names(url$query) != "facets"],
    facet_list[-length(facet_list)])
  .data$url <- url_build(url)
  
  # check number of facets
  n_facets <- check_facet_count(.data, warn = FALSE)
  
  # incorporate this into the query
  url <- url_parse(.data$url)
  url$query$flimit <- max(n_facets)
  .data$url <- url_build(url)
    
  # run query to get list of count tibbles
  result <- query_API(.data)
  if(is.null(result)){system_down_message("count")}
  result <- lapply(result, 
                   function(a){a$fieldResult |> bind_rows()})
  names(result) <- facet_names[-length(facet_names)]
  
  # expand to a tibble that gives all combinations
  kept_facets <- facet_names[-length(facet_names)]
  result_list <- lapply(names(result), 
         function(a){
           x <- result[[a]][c(1, 4)]
           names(x)[1] <- a
           x}) 
  
  # convert to all combinations of levels
  if(length(result_list) > 1){
    levels_list <- lapply(result_list, function(a){a[[1]]})
    names(levels_list) <- names(result)
    levels_list <- c(levels_list, list(stringsAsFactors = FALSE))
    result_df <- do.call(expand.grid, levels_list) |>
      tibble()
    for(i in seq_along(result_list)){
      result_df <- full_join(result_df, result_list[[i]], by = kept_facets[i])
    }
  }else{
    result_df <- result_list[[1]]
  }
 
  # extract existing fq statements
  query <- url_parse(.data$url)$query
  if(is.null(query$fq)){
    fqs <- ""
  }else{
    fqs <- strsplit(query$fq, "AND")[[1]]
    fqs <- fqs[!grepl(paste(kept_facets, collapse = "|"), fqs)] # remove fqs that relate to parsed facets
    if(length(fqs) < 1){
      fqs <- ""
    }
  }
  
  # glue `fq` statements together 
  result_df <- result_df |>
    rowwise() |>
    mutate(query = glue_collapse(c_across(starts_with("fq")), sep = " AND ")) |>
    select(-starts_with("fq")) |>
    ungroup()
  if(fqs != ""){
    result_df$query <- glue("{fqs} AND {result_df$query}")
  }
  
  # recombine into urls
  url_final <- url_parse(.data$url)
  query_without_fq <- c(
    url_final$query[!(names(url_final$query) %in% 
                      c("fq", "facets", "flimit", "foffset"))],
    list(facets = facet_names[length(facet_names)]),
    saved_facet_queries)
  
  url_list <- lapply(result_df$query, function(a, url){
    url$query <- c(list(fq = a), query_without_fq)
    url_build(url)
  }, url = url_final)
  
  result_df$url <- unlist(url_list)
  result_df <- select(result_df, -query)
  return(result_df)
}