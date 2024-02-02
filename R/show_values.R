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
#' @importFrom cli col_yellow
#' @examples \dontrun{
#' # Show values in field 'cl22'
#' search_fields("cl22") |> 
#'   show_values()
#' 
#' # This is synonymous with `request_metadata() |> unnest()`.
#' # For example, the previous example can be run using:
#' request_metadata() |>
#'   filter(field == "cl22") |>
#'   unnest() |>
#'   collect() 
#' 
#' # Search for any values in field 'cl22' that match 'tas'
#' search_fields("cl22") |> 
#'   search_values("tas")
#' 
#' # See items within species list "dr19257"
#' search_lists("dr19257") |> 
#'   show_values()
#' }
#' @export
show_values <- function(df){
  
  check_values_input(df)
  
  type <- attr(df, "call")
  match_column <- switch(type,
                         "fields" = "id",
                         "lists" = "species_list_uid",
                         "profiles" = "shortName",
                         "taxa" = "taxon_concept_id",
                         "uid" # last option selected if above are exhausted
  )
  match_name <- df[[match_column]][1]

  # specify the number matched fields
  # specify for which field the values are displayed
  if(nrow(df) > 1) {
    n_matches <- nrow(df)
    df <- df[1, ]
    inform(
      bullets <- c(
        "!" = glue("Search returned {n_matches} matched {type}."),
        "*" = glue("Showing values for '{match_name}'.")
      ))
  } else {
    if (is.na(match_name)) {
      inform(cli::col_yellow(glue("`search_all()` returned no matched `{type}`.")))
      tibble()
    } else {
    inform(
      bullets <- c(
        # glue("Search returned 1 matched {type}."),
        "*" = glue("Showing values for '{match_name}'.")
      )
    )
    }
  }
  
  request_metadata() |>
    filter({{type}} == {{match_name}}) |>
    unnest() |>
    collect()
}

#' @param query A string specifying a search term. Not case sensitive.
#' @rdname show_values
#' @export search_values
search_values <- function(df, query) {
  
  values_lookup <- show_values(df)
  check_if_missing(query)
  
  values_lookup |>
    search_text_cols(query = query)
}

#' Internal function to check inputs to `show_values()` & `search_values()` 
#' @noRd
#' @keywords Internal
check_values_input <- function(df, error_call = caller_env()) {
  # Check if missing input
  if(missing(df) || is.null(df)) {
    bullets <- c(
      "Missing information for values lookup.",
      i = "Field, profile or list must be provided as a tibble created by `search_all()`.",
      i = "e.g. `search_all(fields, \"year\") |> show_values()`."
    )
    abort(bullets, call = error_call)
  }
  
  # Check that original data.frame is from a `show_all` or `search_all`
  if(is.null(attr(df, "call"))) {
    bullets <- c(
      "Wrong input provided.",
      i = "Must supply a tibble created by `search_all()` or `show_all()`.",
      i = "e.g. `search_all(fields, \"year\") |> show_values()`."
    )
    abort(bullets, call = error_call)
  }
  
  # Input must be from valid `show_all` or `search_all` tibble
  valid_calls <- c("fields", "lists", "profiles", "taxa")
  if(!any(valid_calls == attr(df, "call"))){
    type <- attr(df, "call")
    bullets <- c(
      glue("Can't lookup values for metadata type `{type}`."),
      x = "Values lookup accepts `fields`, `lists`, `profiles` or `taxa`."
    )
    abort(bullets, call = error_call)
  }
}