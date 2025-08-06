#' Compute counts
#' 
#' This is a little different from other `compute` functions; it converts a 
#' `data_query` object (from `collapse(type = "occurrences-count")`) into one or more urls
#' that can be passed verbatim to `query_API()`. This is important when `group_by`
#' has `expand = TRUE`, because it requires an initial call to the atlas to 
#' calculate the requisite URLs. It makes very little difference under other
#' circumstances
#' @param .query An object of class `data_query`
#' @keywords Internal
#' @noRd
collapse_occurrences_count <- function(.query){
  if(is_gbif()){
    if(.query$expand){    
      abort("Grouped counts haven't been (re)implemented for GBIF yet")
      # compute_grouped_counts_GBIF(.query)
    }else{
      collapse_occurreces_count_gbif(.query)
    }
  }else{
    if(.query$expand){
      collapse_occurrences_count_groupby(.query)
    }else{
      collapse_occurrences_count_nogroupby(.query)
    }
  }
}
# Note: the above handles types `data/occurrences-count-groupby` 
# and `data/occurrences-count`. This is probably inefficient.


#' Function to construct `body` arg for GBIF
#' predicates are JSON scripts for passing to GBIF offline downloads API.
#' https://www.gbif.org/developer/occurrence
#' @param x A list with slots relevant to building predicates
#' @noRd
#' @keywords Internal
collapse_occurreces_count_gbif <- function(x){
  # GBIF predicates:
  if(any(names(x) == "body")){
    result <- switch(x$type,
                     "data/occurrences" = {
                       list(
                         creator = potions::pour("user", "username", .pkg = "galah"),
                         notificationAddresses = potions::pour("user", "email", .pkg = "galah"),
                         sendNotification = potions::pour("package", "send_email", .pkg = "galah"),
                         format = x$format,
                         predicate = build_predicates(x$body))
                     },
                     "data/occurrences-count" = {
                       predicates_list <- build_predicates(x$body)
                       if(is.null(predicates_list)){
                         list(limit = 0)
                       }else{
                         list(
                           predicate = predicates_list,
                           limit = 0)                         
                       }
                     },
                     "data/occurrences-count-groupby" = {
                       browser()
                       # note there is a `facet` arg suggested in the API docs, may work here
                     }
    ) |>
      jsonlite::toJSON(auto_unbox = TRUE,
                       pretty = TRUE)
    x$body <- result
  }
  x
}

#' Internal function to handle facet counting, adjustment etc.
#' @noRd
#' @keywords Internal
collapse_occurrences_count_nogroupby <- function(.query){
  url <- httr2::url_parse(.query$url)
  
  # check if a limit has been set
  if(!is.null(url$query$flimit)){
    
    # check number of total facets in the field
    n_facets <- check_facet_count(.query)
    
    # handle slice_head
    if(as.integer(url$query$flimit) < 1){
      url$query$flimit <- .query$arrange$slice_n # Q: is this correct? 
      if(.query$arrange$slice_n < n_facets){
        url$query$foffset <- n_facets - .query$arrange$slice_n
      }
      .query$url <- httr2::url_build(url) 
      .query
    
    # message when limit is hit
    }else{
      if(as.integer(url$query$flimit) < n_facets){
        limit <- url$query$flimit |> prettyNum(big.mark=",", preserve.width="none")
        n_total_facets <- n_facets |> prettyNum(big.mark=",", preserve.width="none")
        
        c(
          cli::cli_text(cli::col_yellow(glue("Limiting to first {limit} of {n_total_facets} rows."))),
          cli::cli_text(cli::col_magenta("Use `atlas_counts(limit = )` to return more rows."))
        ) |>
        cli::cli_inform()
      }
      # .query$url <- url_build(url) 
      .query
    }
  }else{
    .query
  }
}

#' Determine set of queries when expand = TRUE
#' @noRd
#' @keywords Internal
collapse_occurrences_count_groupby <- function(.query, 
                                               error_call = caller_env()){
  data_cached <- .query
  # get url
  url <- httr2::url_parse(.query$url)

  # remove last-provided facet
  facet_list <- url$query[names(url$query) == "facets"]
  facet_names <- unlist(facet_list)
  names(facet_names) <- NULL
  
  # Error when Atlas facet max is reached
  if (pour("atlas", "region") == "Estonia" & 
      length(facet_list) > 1) {
    atlas <- pour("atlas", "region")
    n_fields <- length(facet_list)
    c(
      "Too many fields passed to `group_by()`.",
      x = "Selected atlas ({atlas}) accepts a maximum of 1 field, not {n_fields}.") |>
    cli::cli_abort(call = error_call)
  }

  # save out facet limits to add back later
  saved_facet_queries <- list( 
    flimit = url$query$flimit,
    foffset = url$query$foffset)
  saved_facet_queries <- saved_facet_queries[
    unlist(lapply(saved_facet_queries, function(a){!is.null(a)}))]
  
  # rebuild url with only first facet argument
  url$query <- c(
    url$query[names(url$query) != "facets"],
    facet_list[-length(facet_list)])
  .query$url <- httr2::url_build(url)
  
  # check number of facets
  n_facets <- check_facet_count(.query, warn = FALSE)
  
  # incorporate this into the query
  url <- httr2::url_parse(.query$url)
  url$query$flimit <- max(n_facets)
  .query$url <- httr2::url_build(url)
    
  # run query to get list of count tibbles
  result <- query_API(.query)
  if(is.null(result)){system_down_message("count")}
  result <- purrr::map(result, 
                       \(a){a$fieldResult |> 
                             dplyr::bind_rows()})
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
    levels_list <- purrr::map(result_list, \(a){a[[1]]})
    names(levels_list) <- names(result)
    levels_list <- c(levels_list, list(stringsAsFactors = FALSE))
    result_df <- do.call(expand.grid, levels_list) |>
      tibble::tibble()
    for(i in seq_along(result_list)){
      result_df <- dplyr::full_join(result_df, 
                                    result_list[[i]], 
                                    by = kept_facets[i])
    }
  }else{
    result_df <- result_list[[1]]
  }
 
  # # extract existing fq statements
  query <- httr2::url_parse(.query$url)$query
  if(is.null(query$fq)){
    fqs <- NULL
  }else{
    fqs <- strsplit(query$fq, "AND")[[1]]
    fqs <- fqs[!grepl(glue::glue_collapse(kept_facets, sep = "|"), fqs)] # remove fqs that relate to parsed facets
    if(length(fqs) < 1){
      fqs <- NULL
    }else{
      fqs <- glue::glue_collapse(fqs, sep = " AND ")
    }
  }
  
  # glue `fq` statements together 
  result_df <- result_df |>
    dplyr::rowwise() |>
    dplyr::mutate(query = glue::glue_collapse(dplyr::c_across(dplyr::starts_with("fq")), sep = " AND ")) |>
    dplyr::select(-dplyr::starts_with("fq")) |>
    dplyr::ungroup()
  if(!is.null(fqs)){
    result_df$query <- glue::glue("{fqs} AND {result_df$query}")
  }
  
  # recombine into urls
  url_final <- httr2::url_parse(.query$url)
  query_without_fq <- c(
    url_final$query[!(names(url_final$query) %in% 
                      c("fq", "facets", "flimit", "foffset"))],
    list(facets = facet_names[length(facet_names)]),
    saved_facet_queries)
  
  url_list <- lapply(result_df$query, function(a, url){
    url$query <- c(list(fq = a), query_without_fq)
    httr2::url_build(url)
  }, url = url_final)
  
  result_df$url <- unlist(url_list)
  result_df <- dplyr::select(result_df, -query)

  # join and export
  result <- c(list(
    type = data_cached$type,
    url = result_df),
    data_cached[!(names(data_cached) %in% c("url", "type"))])
  class(result) <- "query"
  result
}

#' Internal function to check number of facets to be returned by a `group_by` query
#' It is called exclusively by `compute_counts()`
#' @noRd 
#' @keywords Internal
check_facet_count <- function(.query, warn = TRUE, error_call = caller_env()){
  url <- httr2::url_parse(.query$url)
  current_limit <- url$query$flimit
  
  if(is.null(current_limit)){
    n_requested <- as.integer(30)
  }else{
    current_limit <- as.integer(current_limit)
    if(current_limit < 1){
      n_requested <- as.integer(30)  
    }else{
      n_requested <- current_limit
    }
  }
  url$query$flimit <- 0
  temp_data <- .query
  temp_data$url <- httr2::url_build(url)
  temp_data$slot_name <- NULL
  result <- query_API(temp_data)
  if(length(result) < 1){
    0
  }else{
    purrr::map(result, function(a){a$count}) |> unlist()
  }
  # if(inherits(result, "data.frame")){ # group_by arg present
  #   n_available <- result$count
  #   if(length(n_available) > 1){ # is this correct? What about multiple fields?
  #     n_available
  #   }else{
  #     n_available <- n_available[[1]]
  #     if(warn &
  #        pour("package", "verbose") & 
  #        n_requested < n_available &
  #        !.query$arrange$slice_called &
  #        .query$arrange$direction == "descending" # for ascending, n_requested is always zero (TRUE?)
  #     ){ 
  #       bullets <- c(
  #         glue("This query will return {n_requested} rows, but there are {n_available} rows in total."),
  #         i = "Use `slice_head()` to set a higher number of rows to return.")
  #       inform(bullets)
  #     }
  #     n_available
  #   }
  # }else{
  #   NA
  # }
}
