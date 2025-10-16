#' Internal function to run `compute()` for 
#' `request_metadata(type = "fields") |> unnest()`
#' @noRd
#' @keywords Internal
collect_fields_unnest <- function(.query,
                                  error_call = rlang::caller_env()){
  facet <- .query |>
    purrr::pluck("url") |>
    httr2::url_parse()
  
  if(is_gbif()){
    # get name of facet in question
    facet <-  purrr::pluck(facet, "query", "facet") # NOTE: "facet" (singular)
    check_missing_fields(facet, call = error_call)
    # get result from API
    .query |>
      query_API() |>
      purrr::pluck(!!!list("facets", 1, "counts")) |>
      dplyr::bind_rows() |>
      dplyr::rename_with(camel_to_snake_case) |>
      dplyr::rename({{facet}} := "name") |>
      parse_select(.query)
    
  }else{ 
    facet <-  purrr::pluck(facet, "query", "facets") # NOTE: "facets" (plural)
    check_missing_fields(facet, call = error_call)
    result <- .query |>
      query_API() |>
      purrr::pluck(!!!list(1, "fieldResult")) |>
      dplyr::bind_rows()
    
    # extract unformatted facet values
    if(nrow(result) > 0){
      result |>
        dplyr::mutate(
          field_name := stringr::str_extract(result$i18nCode, "(?<=\\.).*"),
          .before = 1) |>
        dplyr::rename_with(camel_to_snake_case) |>
        dplyr::rename({{facet}} := "field_name") |>
        parse_select(.query)
    }else{ # i.e. catch empty results
      result
    }
  }
}

#' Microfunction to prevent later failures due to missing field names
#' @noRd
#' @keywords Internal
check_missing_fields <- function(x, call){
  if (x == "NA") {
    cli::cli_abort("No `field` passed to `show_values()`/`search_values()`.",
                   call = call)
  }
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "lists") |> unnest()`
#' @noRd
#' @keywords Internal
collect_lists_unnest <- function(.query){
  result <- query_API(.query) |> 
    dplyr::bind_rows()
  # extract additional raw fields columns
  if (any(colnames(result) %in% "kvpValues")) {
    result <- result |>
      tidyr::unnest_wider("kvpValues") |>
      tidyr::pivot_wider(names_from = "key",
                         values_from = "value")
  }
  result |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    parse_select(.query)
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "profiles") |> unnest()`
#' @noRd
#' @keywords Internal
collect_profiles_unnest <- function(.query){
  result <- query_API(.query)
  result |>
    purrr::pluck("categories") |>
    dplyr::bind_rows() |>
    dplyr::pull("qualityFilters") |>
    dplyr::bind_rows() |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_select(.query)
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @noRd
#' @keywords Internal
collect_taxa_unnest <- function(.query){
  query_API(.query) |>
    dplyr::bind_rows() |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    parse_select(.query)
}
