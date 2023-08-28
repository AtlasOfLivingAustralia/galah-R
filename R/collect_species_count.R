#' Internal function to collect counts for species
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
collect_species_counts <- function(.data){
  result <- query_API(.data)
  tibble(count = result$count)
}