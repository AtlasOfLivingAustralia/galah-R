#' Internal function to compute for species counts
#' @noRd
#' @keywords Interal
compute_species_count <- function(.data){
  class(.data) <- "data_response"
  return(.data)
}