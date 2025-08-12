#' Internal function to run `compute()` for 
#' `request_metadata(type = "fields") |> unnest()`
#' @noRd
#' @keywords Internal
collect_fields_unnest <- function(.query, error_call = caller_env()){
  if(is_gbif()){
    facet <- .query |>
      purrr::pluck("url") |>
      httr2::url_parse() |>
      purrr::pluck("query", "facet")
    
    if (facet == "NA") {
      cli::cli_abort("No `field` passed to `show_values()`/`search_values()`.")
    }
    
    result <- .query |>
      query_API() |>
      purrr::pluck(!!!list("facets", 1, "counts")) |>
      dplyr::bind_rows()
    colnames(result)[which(colnames(result) == "name")[1]] <- facet
    select(result, {{facet}})
    
  }else{ 
    facet <- .query |>
      purrr::pluck("url") |>
      httr2::url_parse() |>
      purrr::pluck("query", "facets")
    
    if (facet == "NA") {
      cli::cli_abort("No `field` passed to `show_values()`/`search_values()`.")
    }

    result <- .query |>
      query_API() |>
      purrr::pluck(!!!list(1, "fieldResult")) |>
      dplyr::bind_rows()
    
    # extract unformatted facet values
    if(nrow(result) > 0){
      result <- result |>
        dplyr::mutate(
          field_value = stringr::str_extract(
            result$i18nCode, 
            "(?<=\\.).*" # everything after .
          )
        )
      
      colnames(result)[which(colnames(result) == "field_value")[1]] <- facet
      dplyr::select(result, {{facet}})      
    }else{ # i.e. catch empty results
      result
    }
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

  return(result)
  
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "profiles") |> unnest()`
#' @noRd
#' @keywords Internal
collect_profiles_unnest <- function(.query){
  result <- query_API(.query) |> 
    purrr::pluck("categories") |>
    dplyr::bind_rows()
  result <- result |>
    dplyr::pull("qualityFilters") |>
    dplyr::bind_rows()
  result
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @noRd
#' @keywords Internal
collect_taxa_unnest <- function(.query){
  query_API(.query) |>
    dplyr::bind_rows()
}
