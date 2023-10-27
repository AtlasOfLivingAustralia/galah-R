#' Internal function to collect counts for species
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
collect_species_count <- function(q_obj){
  result <- query_API(q_obj)
  counts <- lapply(result, extract_species_count) |> unlist()
  if(nrow(q_obj$url) > 1){
    select(q_obj$url, -url) |>
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
