#' Show valid values
#' 
#' @description Within a field, profile or list, there are are valid values that 
#' are then used to narrow or filter queries. A user can check what the valid 
#' categories are within a field, profile or list with this function.
#' 
#' **Fields** contain values corresponding to each field's categories. 
#' These are used to narrow queries with [galah_filter()]. 
#' For example: 
#'   *  The `field` "year" contains values 2021, 2020, 2019, etc
#'   *  The `field` "stateProvince" contains values New South Wales, Victoria, Queensland, etc
#'   
#' **Profiles** contain values corresponding to individual filters that 
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
  
  # Error if `show_values` is piped from a `search_` function
  if(is.null(attr(df, "call"))) {
    bullets <- c(
      "Unrecognised input to `show_values()`",
      i = "Must pipe `show_values()` from a `search_` function."
    )
    abort(bullets, call = caller_env())
  }
  
  # Provide useful error is wrong `search_` function is used
  if(!grepl("^search_", attr(df, "call")) || 
     !(attr(df, "call") %in% c("search_fields", "search_profiles", "search_lists"))){
    bullets <- c(
      "`show_values()` can't use supplied information type.",
      i = "Values can only be provided for fields, profiles & lists."
    )
    abort(bullets, call = caller_env())
  }
  
  # Get correct information 'type'
  if(attr(df, "call") %in% "search_fields") {
    type <- "field"
  } else {
    if(attr(df, "call") %in% "search_profiles") {
      type <- "profile"
    } else {
      if(attr(df, "call") %in% "search_lists") {
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
  
  ## TODO: if more than 1 row returned for search, send helpful error.
  
  # capture initial search query
  entry <- paste(attr(df, "search"))
  
  # use do.call to implement sub-function
  args <- list(entry)
  names(args)[[1]] <- type
  do.call(paste0("show_", type, "_values"), args)

}