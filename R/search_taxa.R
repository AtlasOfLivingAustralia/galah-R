#' @param ... : One or more scientific names, separated by commas and
#' given as strings. If greater control is required to disambiguate search
#' terms, taxonomic levels can be provided explicitly via a `data.frame` 
#' (see examples). Note that searches are not case-sensitive.
#' @rdname search_minifunctions
#' @export
search_taxa <- function(...) {
  
  query <- list(...)
  if(length(query) < 1){
    warn("No query passed to `search_taxa`")
    return(tibble())
  } else if(length(query) == 1L){
    query <- query[[1]]
    if (is.list(query) & !is.data.frame(query)) {
      query <- as.data.frame(query)
    }
  } else {
    if(
      all(lengths(query) == 1L) | 
      all(unlist(lapply(query, is.character)))
    ){
      query <- unlist(query)
    }
  } 
 
  matches <- remove_parentheses(query) |> 
    name_query() |>
    as_tibble()
    
  if(is.null(matches) & galah_config()$verbose){
    bullets <- c(
      "Calling the API failed for `search_taxa`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct.",
      i = "If you continue to see this message, please email support@ala.org.au."
    )
    inform(bullets)
    df <- tibble()
    attr(df, "call") <- "ala_id"
    return(df)
  }else{
    attr(matches, "call") <- "ala_id"
    return(matches)
  } 
}


remove_parentheses <- function(x){
  if(inherits(x, "data.frame")){
    as.data.frame(lapply(x, function(a){stringr::str_remove_all(a, "[()]")}))
  }else{
    stringr::str_remove_all(x, "[()]")
  }
}


name_query <- function(query) {
  if (is.data.frame(query)) {
    matches <- lapply(split(query, seq_len(nrow(query))), name_lookup)
  } else {
    matches <- lapply(query, name_lookup)
  } 
  if(all(unlist(lapply(matches, is.null)))){
    return(NULL)
  }else{
    return(
      as_tibble(rbindlist(matches, fill = TRUE))
    )
  }
}


name_lookup <- function(name) {
  url <- server_config("name_matching_base_url")
  if (is.null(names(name)) || isTRUE(names(name) == "")) {
    # search by scientific name
    path <- "api/search"
    query <- list(q = name[[1]])
  } else {
    # search by classification
    path <- "api/searchByClassification"
    name <- validate_rank(name)
    query <- as.list(name)
  }
  result <- atlas_GET(url, path, query)
  
  if(is.null(result)){
    return(NULL)
  }

  if ("homonym" %in% result$issues) {
    bullets <- c(
      "Your search returned multiple taxa due to a homonym issue.",
      i = "Please provide another rank in your search to clarify taxa.",
      x = glue("Homonym issue with \"{name}\".")
    )
    warn(bullets)
    # return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
  }  #else 
  if (isFALSE(result$success) && galah_config()$verbose) {
    list_invalid_taxa <- glue::glue_collapse(name, 
                                             sep = ", ")
    inform(glue("No taxon matches were found for \"{list_invalid_taxa}\"."))
    return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
  }
  names(result) <- rename_columns(names(result), type = "taxa")

  # if search term includes more than one rank, how to include in output?
  if (length(name) > 1) {
    name <- paste(unname(unlist(name)), collapse  = "_")
  }
  cbind(search_term = name,
        as.data.frame(result[names(result) %in% wanted_columns("taxa")],
                      stringsAsFactors = FALSE))
}


# make sure rank provided is in accepted list
validate_rank <- function(df) { 
  ranks <- names(df)
  ranks_check <- ranks %in% show_all_ranks()$name
  if(any(ranks_check)){
    return(df[ranks_check])
  }else{
    return(NULL)
  }
}