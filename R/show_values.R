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
#' @importFrom tibble tibble
#' @examples \donttest{
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
  # NOTE: the below assumes that each `show_all()` function
  # returns the columsn `type` and `id`; 
  # this may need to be reverse engineered
  x <- request_values() 
  x$filter <- tibble(api = df$type[[1]],
                     selection = df$id[[1]])
  # note: it would be better to have 
  # filter({{type}} == {{id}})
  # ...but this fails for some reason
  collect(x)
}

#' @param query A string specifying a search term. Not case sensitive.
#' @rdname show_values
#' @export search_values
search_values <- function(df, query) {
  result <- show_values(df)
  # add a query section here using `grepl`
  # probably good to suggest people use filter.data.frame for this
}

# below here likely to be unnecessary once above code is working

# internal functions for values look-up ----------------------------------------
show_values_field <- function(field) {
  if (missing(field) || is.null(field)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a field to show values for?")
    abort(bullets, call = caller_env())
  }
  request_values() |>
    filter(field == field) |>
    collect()
}

#' @importFrom rlang caller_env
#' @importFrom rlang warn
#' @noRd
#' @keywords Internal
search_values_field <- function(field, query){
  if (missing(query) || is.null(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    rlang::warn(message = bullets, error = caller_env())
  }
  field_text <- show_values_field(field)
  field_text[grepl(query, tolower(field_text[[1]])), ]
}

# up to here

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
    c("collection", "dataset", "field", "list", "profile", "provider"))
  valid_calls <- apply(calls_df, 1, function(x){paste0(paste(x, collapse = ""), "s")})
    
  if(!any(valid_calls == attr(df, "call"))){
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
