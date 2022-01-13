#' Taxon information
#'
#' In the ALA, all records are associated with an identifier that uniquely
#' identifies the taxon to which that record belongs. However, taxonomic names
#' are often ambiguous due to homonymy; i.e. re-use of names (common or
#' scientific) in different clades. Hence, `search_taxa` provides a means
#' to search for taxonomic names and check the results are 'correct' before
#' proceeded to download data via [atlas_occurrences()],
#' [atlas_species()] or [atlas_counts()]. The resulting
#' `data.frame` of taxonomic information can be passed directly to
#' `atlas_` functions to filter records to the specified taxon or taxa.
#'
#' @param ... : A vector containing one or more search terms,
#' given as strings. Search terms can be scientific or common names, or
#' taxonomic identifiers. If greater control is required to disambiguate search
#' terms, taxonomic levels can be provided explicitly via a `data.frame` 
#' (see examples). Note that searches are not case-sensitive.
#' @return An object of class `tbl_df`, `data.frame` (aka a tibble) and `ala_id`
#' containing taxonomic information.
#' @seealso [find_taxa()] for how to get names if taxonomic identifiers are 
#' already known. [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for other ways to restrict the information returned
#' by [atlas_occurrences()] and related functions.
#' [atlas_taxonomy()] to look up taxonomic trees.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Search using a single term
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa("Reptilia")
#' ```
#' 
#' Note that `search_taxa()` is not case sensitive
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa("reptilia") # not case sensitive
#' ```
#'
#' Search multiple taxa. `search_taxa()` will return one row per taxon
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa(c("reptilia", "mammalia"))
#' ```
#' 
#' Use `search_taxa()` to narrow your data queries
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' atlas_counts(taxa = search_taxa("reptilia"))
#' ```
#' 
#' You can also use `search_taxa()` with pipes, using either `%>%` or `|>`. 
#' Just begin with [galah_call()]
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() %>%
#'   search_taxa("reptilia") %>%
#'   atlas_counts()
#' ```
#' 
#' @export
search_taxa <- function(...) {
  
  verbose <- getOption("galah_config")$verbose
  
  if (getOption("galah_config")$atlas != "Australia") {
    international_atlas <- getOption("galah_config")$atlas
    bullets <- c(
      "`search_taxa` only provides information on Australian taxonomy.",
      i = glue("To search taxonomy for {international_atlas} use `taxize`."),
      i = "See vignette('international_atlases' for more information."
    )
    rlang_abort(bullets)
  }
  
  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")   
  checked_dots <- detect_data_request(dots)

  if(!inherits(checked_dots, "quosures")){
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  }else{
    is_data_request <- FALSE
  }
  
  # check for missing queries
  if(length(dots) < 1){
    bullets <- c(
      "Query is missing, with no default.",
      i = "`search_taxa` requires a query to search for."
    )
    rlang_abort(bullets)
  }
  # capture named inputs
  check_queries(dots) 

  # convert dots to query
  query <- parse_basic_quosures(dots)
   
  if (is.list(query)
  # && length(names(query)) > 0 
  ) {
    query <- as.data.frame(query) # convert to dataframe for simplicity
  }
  
  matches <- remove_parentheses(query) |> 
    name_query() |>
    set_galah_object_class(class = "ala_id")

  if(is.null(matches) & galah_config()$verbose){
    bullets <- c(
      "Calling the API failed for `search_taxa`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct.",
      i = "If you continue to see this message, please email support@ala.org.au."
    )
    inform(bullets)
    return(set_galah_object_class(class = "ala_id"))
  }else{
    # if a data request was supplied, return one
    if(is_data_request){
      update_galah_call(data_request, taxa = matches)
    }else{
      matches
    }   
  } 
}


# checker function based on `galah_filter.R/check_filters`
check_queries <- function(dots) {
  if(any(have_name(dots))){
    if(any(names(dots) == "children")){  # formerly an option under `select_taxa`   
      bullets <- c(
        "Invalid option entered for `search_taxa`.",
        i = "See `?search_taxa` for more information.",
        x = "`children` is not a valid option"
      )
      rlang_abort(bullets)     
    }else{
      bullets <- c(
        "We detected a named input.",
        i = glue("This usually means that you've used `=` somewhere"),
        i = glue("`search_taxa` doesn't require equations")
      )
      rlang_abort(bullets)
    }
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