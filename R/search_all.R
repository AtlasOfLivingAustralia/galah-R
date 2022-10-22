#' @name search_all
#' @rdname search_all
#' @title Search for record information
#' @description 
#' The living atlases store a huge amount of information, above and beyond the 
#' occurrence records that are their main output. In `galah`, one way that 
#' users can investigate this information is by searching for a specific option 
#' or category for the type of information they are interested in. 
#' Functions prefixed with `search_` do this, displaying any matches to a 
#' search term within the valid options for the information specified by the 
#' suffix.
#' 
#' `r lifecycle::badge("experimental")`
#' `search_all()` is a helper function that can do searches within multiple 
#' types of information from `search_` sub-functions. 
#' See `Details` (below) for accepted values.
#' 
#' @param query A string specifying a search term. Searches are not 
#' case-sensitive.
#' @details There are five categories of information, each with their own 
#' specific sub-functions to look-up each type of information. 
#' The available types of information for `search_all()` are:
#' 
#' 
#' | **Category** | **Type** | **Description** | **Sub-functions** |
#' |---|---|---|---|
#' | configuration  |`atlases`| Search for what atlases are available | `search_atlases()`|
#' | |`apis`| Search for what APIs & functions are available for each atlas | `search_apis()`|
#' | |`reasons`| Search for what values are acceptable as 'download reasons' for a specified atlas | `search_reasons()`|
#' | taxonomy | `taxa` | Search for one or more taxonomic names | `search_taxa()` |
#' | |`identifiers`| Take a universal identifier and return taxonomic information | `search_identifiers()` |
#' | |`ranks`| Search for valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.) | `search_ranks()` |
#' | filters |`fields`| Search for fields that are stored in an atlas | `search_fields()` |
#'  | |`assertions`| Search for results of data quality checks run by each atlas | `search_assertions()` |
#' | |`licenses`| Search for copyright licences applied to media | `search_licenses()`|
#' |group filters|`profiles`| Search for what data profiles are available | `search_profiles()` |
#' | |`lists`| Search for what species lists are available| `search_lists()` |
#' |data providers|`providers`| Search for which institutions have provided data | `search_providers()`|
#' | |`collections`|Search for the specific collections within those institutions| `search_collections()`|
#' | |`datasets`|Search for the data groupings within those collections| `search_datasets()`|  
#' 
#' 
#' @aliases search_all
#' @aliases search_atlases search_reasons
#' @aliases search_ranks search_fields search_assertions 
#' @aliases search_profiles search_providers 
#' @aliases search_collections search_datasets search_licences search_apis
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) 
#' containing all data that match the search query.
#' @references 
#' *  Darwin Core terms <https://dwc.tdwg.org/terms/>
#' *  ALA fields <https://api.ala.org.au/#ws72>
#' *  ALA assertions fields <https://api.ala.org.au/#ws81>
#' 
#' @seealso See [search_taxa()] and [search_identifiers()] for more information 
#' on taxonomic searches. 
#' Use the [show_all()] function and `show_all_()` sub-functions to 
#' show available options of information. These functions are used to pass valid 
#' arguments to [galah_select()], [galah_filter()], and related functions.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Search for all fields that use include the word "date"
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' search_all(fields, "date")
#' ```
#' 
#' Search for all fields that have information for "marine"
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' search_all(fields, "marine")
#' ```
#' 
#' Search using a single term (See [search_taxa()] for more info)
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa("Reptilia")
#' ```
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' search_all(taxa, "Reptilia") # equivalent
#' ```
#' 
#' Look up a unique taxon identifier (See [search_identifiers()] for more info)
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' search_all(identifiers, 
#'            "https://id.biodiversity.org.au/node/apni/2914510") # equivalent
#' ```
#' 
#' Search for species lists that match "endangered"
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' search_all(lists, "endangered") # equivalent
#' ```
#' 
#' Search for a valid taxonomic rank, "subphylum"
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' search_all(ranks, "subphylum")
#' ```
#' 
NULL


#' search atlas metadata
#' @param type A string to specify what type of parameters should be searched.
#' @importFrom utils adist
#' @rdname search_all
#' @export search_all
search_all <- function(type, query){
  
  # vector of valid types for this function
  valid_types <- c(
    "ranks",
    "fields", "assertions",
    "licences",
    "profiles", "lists",
    "atlases", "apis", "reasons", 
    "taxa",
    "providers", "collections", "datasets")
  # show_all_cached_files?
  
  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- enquos(type) |> parse_objects_or_functions()   
    type <-  gsub("\"", "", as_label(type[[1]]))
    assert_that(is.character(type))
    check_type_valid(type, valid_types)   
  }
  
  # check for query
  check_if_missing(query)
  
  # run query
  df <- do.call(paste0("search_", type), args = list(query = query))
  
  # attach correct 'search_' class attribute
  attr(df, "call") <- paste0("search_", type)
  return(df)
  
}


#' @rdname search_all
#' @export search_apis
search_apis <- function(query){
  check_if_missing(query)
  df <- node_config
  attr(df, "call") <- "search_apis"
  df_string <- apply(
    df[, c("atlas", "system", "api_name", "called_by")], 
    1, 
    function(a){paste(a, collapse = " ")})
  df[grepl(tolower(query), tolower(df_string)), ]
}


#' @rdname search_all
#' @export search_assertions
search_assertions <- function(query){
  check_if_missing(query)
  df <- show_all_assertions()
  attr(df, "call") <- "search_assertions"
  df[with(df, grepl(tolower(query), 
                    paste(tolower(df$description), tolower(df$id)))), ]
}

#' @rdname search_all
#' @export search_atlases
search_atlases <- function(query){
  check_if_missing(query)
  df <- show_all_atlases()
  attr(df, "call") <- "search_atlases"
  df[grepl(
    tolower(query), 
    tolower(apply(
      df[, c("acronym", "atlas")], 1, 
      function(a){paste(a, collapse = "-")})
    )
  ), ]
}


#' @rdname search_all
#' @export search_collections
search_collections <- function(query){
  check_if_missing(query)
  df <- show_all_collections()
  attr(df, "call") <- "search_collections"
  df[with(df, grepl(tolower(query), 
                    paste(tolower(df$name), tolower(df$uid)))), ]
}


#' @rdname search_all
#' @export search_datasets
search_datasets <- function(query){
  check_if_missing(query)
  df <- show_all_datasets()
  attr(df, "call") <- "search_datasets"
  df[with(df, grepl(tolower(query), 
                    paste(tolower(df$name), tolower(df$uid)))), ]
}


#' @rdname search_all
#' @export search_providers
search_providers <- function(query){
  check_if_missing(query)
  df <- show_all_providers()
  attr(df, "call") <- "search_providers"
  df[with(df, grepl(tolower(query), 
                    paste(tolower(df$name), tolower(df$uid)))), ]
}


#' @rdname search_all
#' @export search_fields
search_fields <- function(query){
  
  if (missing(query) || is.null(query)) {
    as.data.frame(
      matrix(nrow = 0, ncol = 4, 
             dimnames = list(NULL, c("id", "description", "type", "link")))
    )
    bullets <- c(
      "We didn't detect a field to search for.",
      i = "Try entering text to search for matching fields.",
      i = "To see all valid fields, use `show_all_fields()`."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  } else {
    df <- show_all_fields()
    attr(df, "call") <- "search_fields"
    attr(df, "search") <- {{query}}
    
    # merge information together into searchable strings
    df_string <- tolower(
      apply(
        df[, seq_len(min(c(2, ncol(df))))], 
        1, 
        function(a){paste(a, collapse = " ")}))
    
    # return result of a grepl query
    df <- df[grepl(tolower(query), df_string), ] |> 
      as_tibble()
    
    # calculate similarity of results to query, reorder results
    similarity <- adist(df$id, query)[, 1]
    # browser()
    # similarity <- mapply(stringdist::afind, df$description, query)
    df <- df[order(similarity), ]
    
    # return results in order of similarity to search term
    return(df)
  }
}


#' @rdname search_all
#' @export search_licences
search_licences <- function(query){
  check_if_missing(query)
  df <- show_all_licences()
  attr(df, "call") <- "search_licences"
  df[grepl(
    tolower(query), 
    tolower(
      apply(df[, c("name", "acronym")], 
            1, 
            function(a){paste(a, collapse = " ")})
    )
  ), ]
}


#' @rdname search_all
#' @export search_reasons
search_reasons <- function(query){
  check_if_missing(query)
  df <- show_all_reasons()
  attr(df, "call") <- "search_reasons"
  df[grepl(tolower(query), tolower(df$name)), ]
}


#' @rdname search_all
#' @export search_ranks
search_ranks <- function(query){
  check_if_missing(query)
  df <- show_all_ranks()
  attr(df, "call") <- "search_ranks"
  df[grepl(tolower(query), tolower(df$name)), ]
}


#' @rdname search_all
#' @export search_profiles
search_profiles <- function(query){
  check_if_missing(query)
  df <- show_all_profiles()
  attr(df, "call") <- "search_profiles"
  attr(df, "search") <- {{query}}
  text_string <- apply(df[, -1], 1, function(a){paste(a, collapse = " ")})
  
  # return result of grepl query
  df <- df[grepl(tolower(query), tolower(text_string)), ]
  
  # calculate similarity of results to query, reorder results
  similarity <- adist(df$name, query, ignore.case = TRUE)[, 1]
  df <- df[order(similarity), ]
  
  # return results
  return(df)
}


#' @rdname search_all
#' @export search_lists
search_lists <- function(query){
  check_if_missing(query)
  df <- show_all_lists()
  attr(df, "call") <- "search_lists"
  attr(df, "search") <- {{query}}
  query <- tolower(query)
  
  # return result of grepl query
  df <- df[
    grepl(query, tolower(df$listName)) |
    grepl(query, tolower(df$dataResourceUid)), ]
  
  # calculate similarity of results to query, reorder results
  similarity <- adist(df$listName, query, ignore.case = TRUE)[, 1]
  df <- df[order(similarity), ]
  
  # return results
  return(df)
}


# internal functions ----------------------------------------

check_if_missing <- function(query, error_call = caller_env()) {
  if (missing(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    abort(bullets, call = caller_env())
  }
}