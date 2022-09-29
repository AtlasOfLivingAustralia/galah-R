#' Show values of a specific field
#' 
#' @description Some types of information have categorical values or items that 
#' users need to know. `show_values()` provides users with this information
#' 
#' **Fields** contain values corresponding to the categorical or numeric value 
#' attached to a record.
#' For example: 
#'   *  The `field` "year" contains values 2021, 2020, 2019, etc.
#'   *  The `field` "stateProvince" contains values New South Wales, Victoria, Queensland, etc.
#' These are used to narrow queries with [galah_filter()]. 
#' 
#' **Profiles** contain values corresponding to the individual filters that 
#' make-up each profile's overall set of data quality filters. 
#' For example, the "ALA" profile includes filters that:
#'   *  Exclude all records where spatial validity is FALSE
#'   *  Exclude all records with a latitude value of zero
#'   *  Exclude all records with a longitude value of zero
#' 
#' **Lists** contain values corresponding to each list's specific items. 
#' For example, the Endangered Plant species list contains values including:
#'   *  Acacia curranii (Curly-bark Wattle)
#'   *  Brachyscome papillosa (Mossgiel Daisy)
#'   *  Solanum karsense (Menindee Nightshade)
#' 
#' @param df A search result from [search_fields()], [search_profiles()] or 
#' [search_lists()].
#' @param type The type of information, automatically generated from the 
#' `search_` query.
#' @param entry The query searched, automatically generated from the `search_` query
#' @return A `tibble` of values
#' @section Examples: 
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' See categorical values within field "cl22"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("cl22") |> show_values()
#' ```
#' 
#' See individual filters within data quality profile "ALA"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_profiles("ALA") |> show_values()
#' ```
#' 
#' See items within species list "dr19257"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_lists("dr19257") |> show_values()
#' ```
#' 
#' @export
show_values <- function(df, type, entry){
  
  # Error if `show_values` is not piped from a `search_` function
  if(is.null(attr(df, "call"))) {
    type <- paste(attr(df, "call"))
    bullets <- c(
      "Wrong input to `show_values()`",
      i = "Input must from a tibble created by `search_` or `show_all_` functions."
      )
    abort(bullets, call = caller_env())
  }
  
  # Input must be from valid `show_all` or `search_all` tibble
  if(
     !(attr(df, "call") %in% c("search_fields", "search_profiles", "search_lists", 
                               "show_all_fields", "show_all_profiles", "show_all_lists"))){
    type <- paste(attr(df, "call"))
    # type <- stringr::word(paste(attr(df, "call")), 2, sep="_")
    bullets <- c(
      "Unsupported 'type' for `show_values()`.",
      i = "Must supply a search for a field, profile or list.",
      x = glue("Can't show values for `{type}`.")
    )
    abort(bullets, call = caller_env())
  }
  
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

  # vector of valid types for this function
  valid_types <- c("field", "profile", "list")

  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- enquos(type) |> parse_objects_or_functions()   
    type <-  gsub("\"", "", as_label(type[[1]]))
    assert_that(is.character(type))
    check_type_valid(type, valid_types)   
  }
  
  # inform user about values displayed
  # extract relevant matched names
  if(type == "field") {
    match_name <- df$id[1]
  } else {
    if(type == "list") {
      match_name <- df$dataResourceUid[1]
    } else {
      if(type == "profile") {
        match_name <- df$shortName[1]
      }}}
    
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