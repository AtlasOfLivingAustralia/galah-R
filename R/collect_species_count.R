#' Internal function to collect counts for species
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
collect_species_count <- function(.query){
  result <- query_API(.query)
  counts <- lapply(result, extract_species_count) |> unlist()
  if(nrow(.query$url) > 1){
    select(.query$url, -url) |>
      bind_cols(tibble(count = counts))
  }else{
    tibble(count = counts)
  }
}

#' Internal function to extract counts for species
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
extract_species_count <- function(x){
  pluck(x, !!!list(1, "count"))
}
