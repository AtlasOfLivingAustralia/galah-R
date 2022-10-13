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
#' # Show values in field 'cl22'
#' search_fields("cl22") |> 
#'   show_values()
#' 
#' # Search for any values in field 'cl22' that match 'tas'
#' search_fields("cl22") |> 
#'   search_values("tas")
#' 
#' # See individual filters within data quality profile "ALA"
#' search_profiles("ALA") |> 
#'   show_values()
#' 
#' # See items within species list "dr19257"
#' search_lists("dr19257") |> 
#'   show_values()
#' 
#' @export
show_values <- function(df){
  
  # Check inputs
  check_inputs_to_values(df)
  
  # Get correct information 'type'
  if(attr(df, "call") %in% c("search_fields", "show_all_fields")) {
    type <- "field"
  } else {
    if(attr(df, "call") %in% c("search_profiles", "show_all_profiles")) {
      type <- "profile"
    } else {
      if(attr(df, "call") %in% c("search_lists", 'show_all_lists')) {
        type <- "list"
      }
    }
  }

  # check 'type' is ok
  if(is.null(attr(df, "call"))){
    type <- "fields"
  }
  
  # get first row of matched fields
  if(type == "field") {
    match_name <- df$id[1]
  } else {
    if(type == "list") {
      match_name <- df$dataResourceUid[1]
    } else {
      if(type == "profile") {
        match_name <- df$shortName[1]
      }}}
  
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
  do.call(paste0("show_", type, "_values"), args)

}



#' @param query A string specifying a search term. Not case sensitive.
#' @rdname show_values
#' @export search_values

search_values <- function(df, query) {
  
  # Check for input
  check_inputs_to_values(df)
  
  # Get correct information 'type'
  if(attr(df, "call") %in% c("search_fields", "show_all_fields")) {
    type <- "field"
  } else {
    if(attr(df, "call") %in% c("search_profiles", "show_all_profiles")) {
      type <- "profile"
    } else {
      if(attr(df, "call") %in% c("search_lists", 'show_all_lists')) {
        type <- "list"
      }
    }
  }
  
  # check 'type' is ok
  if(is.null(attr(df, "call"))){
    type <- "fields"
  }
  
  # get first row of matched fields
  if(type == "field") {
    match_name <- df$id[1]
  } else {
    if(type == "list") {
      match_name <- df$dataResourceUid[1]
    } else {
      if(type == "profile") {
        match_name <- df$shortName[1]
      }}}
  
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
  names(args) <- list(type, paste("query"))
  do.call(paste0("search_", type, "_values"), args)
}





# internal functions for values look-up ----------------------------------------

show_field_values <- function(field) {
  if (missing(field)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a field to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  if (!(field %in% show_all_fields()$id)) {
    bullets <- c(
      "Invalid field detected.",
      i = "Search for the valid name of a desired field with `search_fields()`.",
      x = glue("\"{field}\" is not a valid field.")
    )
    abort(bullets, call = caller_env())
  }
  
  url <- atlas_url("records_facets")
  resp <- atlas_GET(url, params = list(facets = field, flimit = 10^4))
  if(is.null(resp)){
    bullets <- c(
      "Calling the API failed for `show_values()`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct."
    )
    inform(bullets)
    return(tibble())
  }else{
    category <- vapply(resp$fieldResult[[1]]$fq, function(n) {
      extract_category_value(n)
    }, USE.NAMES = FALSE, FUN.VALUE = character(1))
    cbind(field = field, as.data.frame(category)) |> as_tibble()
  }
}


search_field_values <- function(field, query){
  
  if (missing(query) || is.null(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  }
  
  field_text <- show_field_values(field)
  field_text[grepl(query, tolower(field_text$category)), ]
}


show_profile_values <- function(profile) {
  
  # check if is numeric or can be converted to numeric
  short_name <- profile_short_name(profile)
  if (is.na(short_name)) {
    bullets <- c(
      "Invalid data quality ID.",
      i = "Use `show_all_profiles` to see a listing of valid profiles.",
      x = glue("{profile} is not a valid ID, short name or name.")
    )
    abort(bullets, call = caller_env())
  }
  
  url <- atlas_url("profiles_lookup", profile = profile)
  resp <- atlas_GET(url)
  if(is.null(resp)){
    bullets <- c(
      "Calling the API failed for `search_profile_attributes`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct."
    )
    inform(bullets)
    tibble()
  }else{
    filters <- rbindlist(resp$categories$qualityFilters)
    subset(filters, select = wanted_columns("quality_filter")) |> as_tibble()
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

search_profile_values <- function(profile, query){
  
  if (missing(query) || is.null(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  }
  
  profile_text <- show_profile_values(profile)
  profile_text[grepl(query, tolower(profile_text$description)), ]
}


show_list_values <- function(list){
  
  if (missing(list)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a list to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  url <- atlas_url("lists_lookup", list_id = list)
  atlas_GET(url) |> tibble()
}


search_list_values <- function(list, query){
  
  if (missing(query) || is.null(query)) {
    bullets <- c(
      "We didn't detect a valid query.",
      i = "Try entering text to search for matching values."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  }
  
  list_text <- show_list_values(list)
  list_text[with(list_text, grepl(tolower(query), 
                                  paste(tolower(list_text$commonName), 
                                        tolower(list_text$scientificName)))), ]
}


# checks inputs to `show_values()` & `search_values()`
check_inputs_to_values <- function(df, error_call = caller_env()) {
  # Check if missing input
  if(missing(df)) {
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
  if(!(attr(df, "call") %in% c("search_fields", "search_profiles", "search_lists", 
                              "show_all_fields", "show_all_profiles", "show_all_lists"))){
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