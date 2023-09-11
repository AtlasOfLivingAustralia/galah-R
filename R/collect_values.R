#' Internal function to run `compute()` for `request_values(type = "collections")`
#' @noRd
#' @keywords Internal
collect_collection_values <- function(.data){
  query_API(.data) |>
    build_tibble_from_nested_list()
}

#' Internal function to run `compute()` for `request_values(type = "datasets")`
#' @noRd
#' @keywords Internal
collect_dataset_values <- function(.data){
  query_API(.data) |>
    build_tibble_from_nested_list()
}

#' Internal function to run `compute()` for `request_values(type = "datasets")`
#' @noRd
#' @keywords Internal
collect_list_values <- function(.data){
  query_API(.data) |> 
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
collect_field_values <- function(.data){
  if(is_gbif()){
    .data |>
      query_API()
    # tibble(result$facets$counts[[1]]) # not updated
  }else{
    facet <- .data |>
      pluck("url") |>
      url_parse() |>
      pluck("query", "facets")
    .data |>
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
collect_profile_values <- function(.data){
  query_API(.data) |> 
    pluck("categories") |>
    bind_rows() |>
    pull(qualityFilters) |>
    bind_rows()
}

#' Internal function to run `compute()` for `request_values(type = "providers")`
#' @noRd
#' @keywords Internal
collect_provider_values <- function(.data){
  query_API(.data) |>
    build_tibble_from_nested_list()
}