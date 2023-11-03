#' Internal function to run `compute()` for `request_values(type = "datasets")`
#' @noRd
#' @keywords Internal
collect_lists_unnest <- function(.query){
  query_API(.query) |> 
    bind_rows()
}

#' Internal function to run `compute()` for `request_values(type = "fields")`
#' @importFrom httr2 url_parse
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_fields_unnest <- function(.query, error_call = caller_env()){
  if(is_gbif()){
    facet <- .query |>
      pluck("url") |>
      url_parse() |>
      pluck("query", "facet")
    
    if (facet == "NA") {
      abort("No `field` passed to `show_values()`/`search_values()`.")
    }
    
    .query |>
      query_API() |>
      pluck(!!!list("facets", 1, "counts")) |>
      bind_rows() |>
      mutate({{facet}} := name) |>
      select({{facet}})
    
  }else{ 
    facet <- .query |>
      pluck("url") |>
      url_parse() |>
      pluck("query", "facets")
    
    if (facet == "NA") {
      abort("No `field` passed to `show_values()`/`search_values()`.")
    }
    
    .query |>
      query_API() |>
      pluck(!!!list(1, "fieldResult")) |>
      bind_rows() |>
      mutate({{facet}} := label) |>
      select({{facet}})
  }
}

#' Internal function to run `compute()` for `request_values(type = "profiles")`
#' @noRd
#' @keywords Internal
collect_profiles_unnest <- function(.query){
  result <- query_API(.query) |> 
    pluck("categories") |>
    bind_rows()
  result <- result |>
    pull("qualityFilters") |>
    bind_rows()
  result
}

#' Internal function to run `compute()` for `request_values(type = "taxa")`
#' @noRd
#' @keywords Internal
collect_taxa_unnest <- function(.query){
  query_API(.query) |>
    bind_rows()
}
