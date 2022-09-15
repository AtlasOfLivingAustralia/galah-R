#' @name search_minifunctions
#' @rdname search_minifunctions
#' @title Search for a valid option within various types of information
#' @description 
#' The living atlases store a huge amount of information, above and beyond the 
#' occurrence records that are their main output. In `galah`, one way that 
#' users can investigate this information is by searching for a specific option 
#' or category for the type of information they are interested in. 
#' Functions prefixed with `search_` do this, displaying any matches to a 
#' search term within the valid options for the information specified by the 
#' suffix.
#' @param query : A string specifying a search term. Searches are not 
#' case-sensitive.
#' @details There are five categories of information, each with their own 
#' sub-functions, or "minifunctions" for to display each type. 
#' The available `search_` sub-functions are:
#' 
#' | **Category** | **Sub-function** | **Description** |
#' |---|---|---|
#' | configuration  |`search_atlases()`| Search for what atlases are available |
#' | |`search_apis()`| Search for what APIs & functions are available for each atlas |
#' | |`search_reasons()`| Show what values are acceptable as 'download reasons' for a specified atlas |
#' | taxonomy | `search_taxa()` | Search for one or more taxonomic names |
#' | |`search_identifiers()`| Take a universal identifier and return taxonomic information |
#' | |`search_ranks()`| Search for valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.) |
#' | filters |`search_fields()`| Search for fields that are stored in an atlas |
#' | |`search_assertions()`| Search for results of data quality checks run by each atlas |
#' | |`search_licences()`| Search for copyright licences applied to media |
#' |group filters|`search_profiles()`| Search for what data profiles are available |
#' | |`search_lists()`| Search for what species lists are available|
#' |data providers|`search_providers()`| Search for which institutions have provided data |
#' | |`search_collections()`|Search for the specific collections within those institutions|
#' | |`search_datasets()`|Search for the data groupings within those collections|
#' 
#' @aliases search_atlases search_reasons
#' @aliases search_ranks search_fields search_values search_assertions 
#' @aliases search_profiles search_profile_attributes search_providers 
#' @aliases search_collections search_datasets search_licences
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble)
#' @seealso [show_all()] and [search_all()] are flexible helper functions to 
#' show and/or search for information returned by each look-up sub-function.
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
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
#' `galah_identify()` uses `search_taxa()` to narrow data queries
#' 
#' Look up a unique taxon identifier
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_identifiers(identifier = "https://id.biodiversity.org.au/node/apni/2914510")
#' ```
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() |>
#'   galah_identify("reptilia") |>
#'   atlas_counts()
#' ```
#' 
#' Search for all fields that use include the word "date"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("date")
#' ```
#' 
#' Search for all fields with the string "basisofrecord"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("basisofrecord")
#' ```
#' 
#' Search for all fields that have information for "marine"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("marine") |> 
#'   head() # only show first 5 results
#' ```
#' 
#' Search for all Wordclim layers
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("worldclim")
#' ```
#'
NULL

#' @rdname search_minifunctions
#' @export search_assertions
search_assertions <- function(query){
  df <- show_all_assertions()
  df[grepl(tolower(query), tolower(df$description)), ]
}

#' @rdname search_minifunctions
#' @export search_atlases
search_atlases <- function(query){
  df <- show_all_atlases()
  df[grepl(
    tolower(query), 
    tolower(apply(
      df[, c("acronym", "atlas")], 1, 
      function(a){paste(a, collapse = "-")})
    )
  ), ]
}


#' @rdname search_minifunctions
#' @export search_collections
search_collections <- function(query){
  df <- show_all_collections()
  df[grepl(tolower(query), tolower(df$name)), ]
}


#' @rdname search_minifunctions
#' @export search_datasets
search_datasets <- function(query){
  df <- show_all_datasets()
  df[grepl(tolower(query), tolower(df$name)), ]
}


#' @rdname search_minifunctions
#' @export search_providers
search_providers <- function(query){
  df <- show_all_providers()
  df[grepl(tolower(query), tolower(df$name)), ]
}


#' @rdname search_minifunctions
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
    
    # merge information together into searchable strings
    df_string <- tolower(
      apply(
        df[, seq_len(min(c(2, ncol(df))))], 
        1, 
        function(a){paste(a, collapse = " ")}))
    
    # return result of a grepl query
    df[grepl(tolower(query), df_string), ] |> 
      as_tibble()
  }
}


#' @rdname search_minifunctions
#' @export search_licences
search_licences <- function(query){
  df <- show_all_licences()
  df[grepl(
    tolower(query), 
    tolower(
      apply(df[, c("name", "acronym")], 
            1, 
            function(a){paste(a, collapse = " ")})
    )
  ), ]
}


#' @rdname search_minifunctions
#' @export search_reasons
search_reasons <- function(query){
  df <- show_all_reasons()
  df[grepl(tolower(query), tolower(df$name)), ]
}


#' @rdname search_minifunctions
#' @export search_ranks
search_ranks <- function(query){
  df <- show_all_ranks()
  df[grepl(tolower(query), tolower(df$name)), ]
}


#' @rdname search_minifunctions
#' @export search_profiles
search_profiles <- function(query){
  df <- show_all_profiles()
  text_string <- apply(df[, -1], 1, function(a){paste(a, collapse = " ")})
  df[grepl(tolower(query), tolower(text_string)), ]
}