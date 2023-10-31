#' Internal function to run `compute()` for `request_values(type = "datasets")`
#' @noRd
#' @keywords Internal
collect_lists_unnest <- function(q_obj){
  query_API(q_obj) |> 
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
collect_fields_unnest <- function(q_obj, error_call = caller_env()){
  if(is_gbif()){
    q_obj |>
      query_API()
  }else{
    facet <- q_obj |>
      pluck("url") |>
      url_parse() |>
      pluck("query", "facets")
    
    if (facet == "NA") {
      abort("No `field` passed to `show_values()`/`search_values()`.")
    }
    
    q_obj |>
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
collect_profiles_unnest <- function(q_obj){
  result <- query_API(q_obj) |> 
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
collect_taxa_unnest <- function(q_obj){
  query_API(q_obj) |>
    bind_rows()
}
