#' Show or search for values within a specified field
#' 
#' @description 
#' Users may wish to see the specific values *within* a chosen field, profile 
#' or list to narrow queries or understand more about the information of 
#' interest. `show_values()` provides users with these values. `search_values()` 
#' allows users for search for specific values within a specified field.
#' 
#' @details
#' Each **Field** contains categorical or numeric values.
#' For example: 
#'   *  The `field` "year" contains values 2021, 2020, 2019, etc.
#'   *  The `field` "stateProvince" contains values New South Wales, Victoria, Queensland, etc.
#' These are used to narrow queries with [galah_filter()]. 
#' 
#' Each **Profile** consists of many individual quality filters. 
#' For example, the "ALA" profile consists of values:
#'   *  Exclude all records where spatial validity is FALSE
#'   *  Exclude all records with a latitude value of zero
#'   *  Exclude all records with a longitude value of zero
#' 
#' Each **List** contains a list of species, usually by taxonomic name. 
#' For example, the Endangered Plant species list contains values: 
#'   *  Acacia curranii (Curly-bark Wattle)
#'   *  Brachyscome papillosa (Mossgiel Daisy)
#'   *  Solanum karsense (Menindee Nightshade)
#' 
#' @param df A search result from [search_fields()], [search_profiles()] or 
#' [search_lists()].
#' @return A `tibble` of values for a specified field, profile or list.
#' @examples 
#' \dontrun{
#' # Show values in field 'cl22'
#' search_fields("cl22") |> 
#'   show_values()
#' 
#' # Search for any values in field 'cl22' that match 'tas'
#' search_fields("cl22") |> 
#'   search_values("tas")
#' 
#' # See items within species list "dr19257"
#' search_lists("dr19257") |> 
#'   show_values()
#' }
#' 
#' @export
show_values <- function(df){
  
  # Check inputs
  check_inputs_to_values(df)
  
  # Get correct information 'type'
  call_suffixes <- c("field", "profile", "list", 
                     "collection", "dataset", "provider")
  call_value <- attr(df, "call")
  call_lookup <- unlist(lapply(call_suffixes, function(a){
    grepl(paste0("_", a, "s$"), call_value)
  }))
  
  if(any(call_lookup)){
    type <- call_suffixes[which(call_lookup)[1]]
  }else{
    type <- "field"
  }
  
  # get first row of matched fields
  match_name <- switch(type,
    "field" = df$id[1],
    "list" = df$dataResourceUid[1],
    "profile" = df$shortName[1],
    df$uid[1] # last option selected if above are exhausted
  )
  
  # specify the number matched fields
  # specify for which field the values are displayed
  if(nrow(df) > 1) {
    n_matches <- nrow(df)
    df <- df[1,]
    inform(
      bullets <- c(
        "!" = glue("Search returned {n_matches} matched {type}s."),
        "*" = glue("Showing values for '{match_name}'.")
        ))
    } else {
    inform(
      bullets <- c(
        # glue("Search returned 1 matched {type}."),
        "*" = glue("Showing values for '{match_name}'.")
      )
    )
  }
  
  # use do.call to implement sub-function
  args <- list(match_name)
  names(args)[[1]] <- type
  do.call(paste0("show_values_", type), args)

}



#' @param query A string specifying a search term. Not case sensitive.
#' @rdname show_values
#' @export search_values

search_values <- function(df, query) {
  
  # Check for input
  check_inputs_to_values(df)
  
  # Get correct information 'type'
  call_suffixes <- c("field", "profile", "list", 
                     "collection", "dataset", "provider")
  call_value <- attr(df, "call")
  call_lookup <- unlist(lapply(call_suffixes, function(a){
    grepl(paste0("_", a, "s$"), call_value)
  }))
  
  if(any(call_lookup)){
    type <- call_suffixes[which(call_lookup)[1]]
  }else{
    type <- "field"
  }
  
  # get first row of matched fields
  match_name <- switch(type,
                       "field" = df$id[1],
                       "list" = df$dataResourceUid[1],
                       "profile" = df$shortName[1],
                       df$uid[1] # last option selected if above are exhausted
  )
  
  # check for query
  check_if_missing(query)
  
  # specify the number matched fields
  # specify for which field the values are displayed
  if(nrow(df) > 1) {
    n_matches <- nrow(df)
    df <- df[1,]
    inform(
      bullets <- c(
        "!" = glue("Search returned {n_matches} matched {type}s."),
        "*" = glue("Showing values for '{match_name}'.")
      ))
  } else {
    inform(
      bullets <- c(
        # glue("Search returned 1 matched {type}."),
        "*" = glue("Showing values for '{match_name}'.")
      )
    )
  }
  
  # run query
  args <- list(match_name, query)
  names(args) <- list(type, "query")
  do.call(paste0("search_values_", type), args)
}





# internal functions for values look-up ----------------------------------------

show_values_field <- function(field) {
  if (missing(field) || is.null(field)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a field to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  if (!(field %in% show_all_fields()$id)) {
    bullets <- c(
      "Unknown field detected.",
      i = "Search for the valid name of a desired field with `search_fields()`."
    )
    abort(bullets, call = caller_env())
  }
  
  if(is_gbif()){
    url <- url_lookup("records_counts")
    resp <- url_GET(url, params = list(facet = field, limit = 0, facetLimit = 10^4))
  }else{
    url <- url_lookup("records_facets")
    resp <- url_GET(url, params = list(facets = field, flimit = 10^4))
  }
  
  if(is.null(resp)){
    system_down_message("show_values")
    return(tibble())
  }else{
    if(is_gbif()){
      tibble(resp$facets$counts[[1]])
    }else{
      category <- vapply(resp$fieldResult[[1]]$fq, function(n) {
        extract_category_value(n)
      }, USE.NAMES = FALSE, FUN.VALUE = character(1))
      cbind(field = field, as.data.frame(category)) |> as_tibble()
    }
  }

}


search_values_field <- function(field, query){
  
  if (missing(query) || is.null(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  }
  
  field_text <- show_values_field(field)
  field_text[grepl(query, tolower(field_text$category)), ]
}


show_values_profile <- function(profile, error_call = caller_env()) {
  
  if (missing(profile) || is.null(profile)) {
    bullets <- c(
      "No profile detected.",
      i = "Did you forget to add a profile to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  # check if is numeric or can be converted to numeric
  short_name <- profile_short_name(profile)[1]
  search_term <- profile
  if (is.na(short_name)) {
    bullets <- c(
      "Unknown profile detected.",
      i = "See a listing of valid data quality profiles with `show_all_profiles()`."
    )
    abort(bullets, call = error_call)
  }
  
  url <- url_lookup("profiles_lookup", profile = profile)
  resp <- url_GET(url)
  if(is.null(resp)){
    system_down_message("show_values")
    tibble()
  }else{
    filters <- bind_rows(resp$categories$qualityFilters)
    subset(filters, select = wanted_columns("quality_filter")) |> tibble()
  }  
}

profile_short_name <- function(profile) {
  valid_profiles <- show_all_profiles()
  short_name <- NA
  if (suppressWarnings(!is.na(as.numeric(profile)))) {
    # assume a profile id has been provided
    short_name <- valid_profiles[match(as.numeric(profile),
                                       valid_profiles$id),]$shortName
  } else {
    # try to match a short name or a long name
    if (profile %in% valid_profiles$name) {
      short_name <- valid_profiles[match(profile,
                                         valid_profiles$name), ]$shortName
    } else {
      if (profile %in% valid_profiles$shortName) {
        short_name <- profile
      }
    }
  }
  short_name
}

search_values_profile <- function(profile, query){
  
  if (missing(query) || is.null(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  }
  
  profile_text <- show_values_profile(profile)
  profile_text[grepl(query, tolower(profile_text$description)), ]
}


show_values_list <- function(list){
  
  if (missing(list) || is.null(list)) {
    bullets <- c(
      "No list detected.",
      i = "Did you forget to add a list to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  url <- url_lookup("lists_lookup", list_id = list)
  url_paginate(url, group_size = 500)
}


search_values_list <- function(list, query){
  
  if (missing(query) || is.null(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  }
  
  list_text <- show_values_list(list)
  list_text[with(list_text, grepl(tolower(query), 
                                  paste(tolower(list_text$commonName), 
                                        tolower(list_text$scientificName)))), ]
}


show_values_collection <- function(collection){
  if (missing(collection) || is.null(collection)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a collection to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  url <- url_lookup("collections_collections") |> paste0("/", collection)
  x <- url_GET(url)
  x[lengths(x) == 1] |> as_tibble()
}

search_values_collection <- function(collection, query){
  bullets <- c(
    "`query` is not defined for collections",
    i = "Use `show_values` instead"
  )
  rlang::warn(message = bullets, error = rlang::caller_env())
  show_values_collection(collection)
}

show_values_provider <- function(provider){
  if (missing(provider) || is.null(provider)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a provider to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  url <- url_lookup("collections_providers") |> paste0("/", provider)
  x <- url_GET(url)
  x[lengths(x) == 1] |> as_tibble()
}

search_values_provider <- function(provider, query){
  bullets <- c(
    "`query` is not defined for providers",
    i = "Use `show_values` instead"
  )
  rlang::warn(message = bullets, error = rlang::caller_env())
  show_values_provider(provider)
}

show_values_dataset <- function(dataset){
  if (missing(dataset) || is.null(dataset)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a dataset to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  url <- url_lookup("collections_datasets") |> paste0("/", dataset)
  x <- url_GET(url)
  x[lengths(x) == 1] |> as_tibble()
}

search_values_dataset <- function(dataset, query){
  bullets <- c(
    "`query` is not defined for datasets",
    i = "Use `show_values` instead"
  )
  rlang::warn(message = bullets, error = rlang::caller_env())
  show_values_dataset(dataset)
}

# checks inputs to `show_values()` & `search_values()`
check_inputs_to_values <- function(df, error_call = caller_env()) {
  # Check if missing input
  if(missing(df) || is.null(df)) {
    bullets <- c(
      "No input detected.",
      i = "Must supply a tibble created by `search_all` or `show_all_` functions."
    )
    abort(bullets, call = error_call)
  }
  
  # Check that original data.frame is from a `show_all` or `search_all`
  if(is.null(attr(df, "call"))) {
    bullets <- c(
      "Wrong input provided.",
      i = "Must supply a tibble created by `search_all` or `show_all_` functions."
    )
    abort(bullets, call = error_call)
  }
  
  # Input must be from valid `show_all` or `search_all` tibble
  calls_df <- expand.grid(
    c("search_", "show_all_"),
    c("field", "profile", "list", "collection", "dataset", "provider"))
  valid_calls <- apply(calls_df, 1, function(x){paste0(paste(x, collapse = ""), "s")})
    
  if(!any(valid_calls == attr(df, "call"))){
    # type <- stringr::word(paste(attr(df, "call")), 2, sep="_")
    type <- attr(df, "call")
    bullets <- c(
      "Unsupported 'type' for values look-up.",
      i = "Must supply a search for a field, profile or list.",
      x = glue("Can't show values for `{type}`.")
    )
    abort(bullets, call = error_call)
  }
}


# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}
