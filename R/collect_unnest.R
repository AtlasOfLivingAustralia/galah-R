#' Internal function to run `compute()` for 
#' `request_metadata(type = "fields") |> unnest()`
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
    
    result <- .query |>
      query_API() |>
      pluck(!!!list("facets", 1, "counts")) |>
      bind_rows()
    colnames(result)[which(colnames(result) == "name")[1]] <- facet
    select(result, {{facet}})
    
  }else{ 
    facet <- .query |>
      pluck("url") |>
      url_parse() |>
      pluck("query", "facets")
    
    if (facet == "NA") {
      abort("No `field` passed to `show_values()`/`search_values()`.")
    }

    result <- .query |>
      query_API() |>
      pluck(!!!list(1, "fieldResult")) |>
      bind_rows()
    colnames(result)[which(colnames(result) == "label")[1]] <- facet
    select(result, {{facet}})
  }
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "lists") |> unnest()`
#' @noRd
#' @keywords Internal
collect_lists_unnest <- function(.query){
  query_API(.query) |> 
    bind_rows()
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "profiles") |> unnest()`
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

#' Internal function to run `compute()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @noRd
#' @keywords Internal
collect_taxa_unnest <- function(.query){
  query_API(.query) |>
    bind_rows()
}
