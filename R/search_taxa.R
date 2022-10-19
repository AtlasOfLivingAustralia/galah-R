#' Look up taxon information
#'
#' Look up taxonomic names before downloading 
#' data from the ALA, using [atlas_occurrences()], [atlas_species()] or 
#' [atlas_counts()]. Taxon information returned by `search_taxa()` may be
#' passed to [galah_identify()] to provide the `identify` argument of 
#' `atlas_` functions. `search_taxa()` allows users to disambiguate homonyms 
#' (i.e. where the same name refers to taxa in different clades) prior to  
#' downloading data. 
#'
#' @param ... : A string of one or more scientific names, separated by commas, 
#' or a data frame specifying taxonomic levels. Note that searches are not 
#' case-sensitive. 
#' 
#' @returns An object of class `tbl_df`, `data.frame` (aka a tibble) and `ala_id`
#' containing taxonomic information.
#' 
#' @seealso [search_identifiers()] for how to get names if taxonomic identifiers 
#' are already known. [galah_identify()], [galah_select()], [galah_filter()], and
#' [galah_geolocate()] for ways to restrict the information returned by
#' [atlas_occurrences()] and related functions. [atlas_taxonomy()] to look 
#' up taxonomic trees.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Search using a single string. Note that `search_taxa()` is not case sensitive
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa("Reptilia")
#' ```
#'
#' Search using multiple strings. `search_taxa()` will return one row per taxon
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa("reptilia", "mammalia")
#' ```
#' 
#' Specify taxonomic levels in a search using a data frame (tibble). Taxa may 
#' be specified using either the specificEpithet argument to designate the second 
#' element of a Latin binomial, or the scientificName argument to specify the 
#' scientific name, which may include the subspecific epithet if required. 
#' 
#'  ```{r, comment = "#>", collapse = TRUE}
#' search_taxa(tibble(
#'   class = "aves", 
#'   family = "pardalotidae", 
#'   genus = "pardalotus", 
#'   specificEpithet = "punctatus"))
#'                       
#' search_taxa(tibble(
#'   family = c("pardalotidae", "maluridae"), 
#'   scientificName = c("Pardalotus striatus striatus", "malurus cyaneus")))
#'  ```
#' 
#' `galah_identify()` uses `search_taxa()` to narrow data queries
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() |>
#'   galah_identify("reptilia") |>
#'   atlas_counts()
#' ```
#'
#' @importFrom utils adist 
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
  
  matches <- remove_parentheses(query) |> name_query()
    
  if(is.null(matches) & galah_config()$verbose){
    system_down_message("search_taxa")
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
    NULL
  }else{
    as_tibble(rbindlist(matches, fill = TRUE))
  }
}


name_lookup <- function(name) {
  if (is.null(names(name)) || isTRUE(names(name) == "")) {
    # search by scientific name
    url <- atlas_url("names_search_single", name = name[[1]])
    result <- atlas_GET(url)
  } else {
    # search by classification - NOTE - NOT implemented for other atlases yet
    url <- atlas_url("names_search_multiple") 
    result <- atlas_GET(url, as.list(name))      
  }

  if(is.null(result)){
    return(NULL)
  }

  # extra processing step for ALA-species - make df-like
  if(is.list(result)){
    if(!is.null(result$searchResults$results)){
      result <- result$searchResults$results
    }else{
      result <- lapply(result, function(a){a[1]}) 
    }
    if(length(result) < 1){
      return(tibble(search_term = name))
    }
  }
  
  # cure issue where some colnames are empty
  col_names <- names(result)
  name_check <- is.na(col_names) | nchar(col_names) < 1
  if(any(name_check)){
    result <- result[!name_check]
  }
  
  # convert to tibble
  result <- as_tibble(result)
  
  if(nrow(result) > 1){
    string_distance <- adist(
      tolower(result$scientificName), 
      tolower(name)
    )[, 1]
    if(any(string_distance < 5)){
      result <- result[which.min(string_distance), ]
      result$success <- TRUE
    }else{
      result <- result[1, ]
      result$success <- FALSE
    }
  }else{
    if(!any(names(result) == "success")){
      result$success <- TRUE
    }
  }

  if(any(colnames(result) == "issues")){
    if ("homonym" %in% result$issues) {
      bullets <- c(
        "Your search returned multiple taxa due to a homonym issue.",
        i = "Please provide another rank in your search to clarify taxa.",
        x = glue("Homonym issue with \"{name}\".")
      )
      warn(bullets)
      # return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
    }
  }
  
  if (isFALSE(result$success) && galah_config()$verbose) {
    list_invalid_taxa <- glue::glue_collapse(name, 
                                             sep = ", ")
    inform(glue("No taxon matches were found for \"{list_invalid_taxa}\" in the selected atlas ({getOption('galah_config')$atlas})."))
    return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
  }

  # update column names
  names(result) <- rename_columns(names(result), type = "taxa")

  # if search term includes more than one rank, how to include in output?
  if (length(name) > 1) {
    name <- paste(unname(unlist(name)), collapse  = "_")
  }
  
  cbind(
    search_term = name,
    as.data.frame(
      result[names(result) %in% wanted_columns("taxa")[1:10]],
      stringsAsFactors = FALSE),
    as.data.frame(
      result[names(result) %in% wanted_columns("taxa")[11:33]],
      stringsAsFactors = FALSE)
  )
}


# make sure rank provided is in accepted list
# validate_rank <- function(df) {
#   ranks <- names(df)
#   ranks_check <- ranks %in% show_all_ranks()$name
#   if(any(ranks_check)){
#     return(df[ranks_check])
#   }else{
#     return(NULL)
#   }
# }