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

  type <- attr(df, "call")
  match_name <- switch(type,
                       "fields" = df$id[1],
                       "lists" = df$dataResourceUid[1],
                       "profiles" = df$shortName[1],
                       df$uid[1] # last option selected if above are exhausted
  )
  
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
    inform(
      bullets <- c(
        # glue("Search returned 1 matched {type}."),
        "*" = glue("Showing values for '{match_name}'.")
      )
    )
  }
  
  # NOTE: the below assumes that each `show_all()` function
  # returns the columns `type` and `id`; 
  # this may need to be reverse engineered
  x <- request_values() 
  x$filter <- tibble(api = type,
                     selection = match_name)
  # note: it would be better to have 
  # filter({{type}} == {{id}})
  # ...but this fails for some reason
  collapse(x) |> 
    collect()
}

#' @param query A string specifying a search term. Not case sensitive.
#' @rdname show_values
#' @export search_values
search_values <- function(df, query) {
  show_values(df) |>
    search_text_cols(query = query)
}