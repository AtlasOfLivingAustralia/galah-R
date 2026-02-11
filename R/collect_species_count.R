#' Internal function to collect counts for species
#' @noRd
#' @keywords Internal
collect_species_count <- function(.query){
  result <- query_API(.query)
  counts <- purrr::map(result, extract_species_count) |> 
    unlist()
  if(nrow(.query$url) > 1){
    .query |>
      purrr::pluck("url") |>
      dplyr::select(-url) |>
      dplyr::bind_cols(tibble::tibble(count = counts))
  }else{
    tibble::tibble(count = counts)
  }
}

#' Internal function to extract counts for species
#' @noRd
#' @keywords Internal
extract_species_count <- function(x){
  purrr::pluck(x, !!!list(1, "count"))
}
