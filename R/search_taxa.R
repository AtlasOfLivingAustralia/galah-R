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
#' @param query `string`: A vector containing one or more search terms,
#' given as strings. Search terms can be scientific or common names, or
#' taxonomic identifiers. If greater control is required to disambiguate search
#' terms, taxonomic levels can be provided explicitly via a named `list`
#' for a single name, or a `data.frame` for multiple names (see examples).
#' Note that searches are not case-sensitive.
#' @return An object of class `tbl_df`, `data.frame` (aka a tibble) and `ala_id`
#' containing taxonomic information.
#' @seealso [find_taxa()] for how to get names if taxonomic identifiers are 
#' already known. [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for other ways to restrict the information returned
#' by [atlas_occurrences()] and related functions.
#' [atlas_taxonomy()] to look up taxonomic trees.
#' @examples
#' # Search using a single term
#' search_taxa("Reptilia")
#' # or equivalently:
#' search_taxa("reptilia") # not case sensitive
#'
#' # Search multiple taxa
#' search_taxa(c("reptilia", "mammalia")) # returns one row per taxon
#' 
#' @export
search_taxa <- function(...) {
  UseMethod("search_taxa")
}

#' @export
#' @rdname search_taxa
search_taxa.data_request <- function(request, ...) {
  request$taxa <- do.call(search_taxa, merge_args(request, list(...)))
  return(request)
}

#' @export
#' @rdname search_taxa
search_taxa.default <- function(query) {

  verbose <- getOption("galah_config")$verbose

  if (getOption("galah_config")$atlas != "Australia") {
    stop("`search_taxa` only provides information on Australian taxonomy. To search taxonomy for ",
         getOption("galah_config")$atlas, " use `taxize`. See vignette('international_atlases') for more information")
  }

  if (missing(query)) {
    stop("`search_taxa` requires a query to search for")
  }
  
  if (is.list(query) && length(names(query)) > 0 ) {
    query <- as.data.frame(query) # convert to dataframe for simplicity
  }
  
  matches <- remove_parentheses(query) |> name_query()

  if(is.null(matches) & galah_config()$verbose){
    inform("Calling the API failed for `search_taxa`")
    return(set_galah_object_class(class = "ala_id"))
  }else{
    set_galah_object_class(matches, "ala_id")
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
      as_tibble(data.table::rbindlist(matches, fill = TRUE))
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
    warning("Homonym issue with ", name,
         ". Please also provide another rank to clarify.")
    # return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
  }  #else 
  if (isFALSE(result$success) && galah_config()$verbose) {
    message("No taxon matches were found for \"", name, "\"")
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