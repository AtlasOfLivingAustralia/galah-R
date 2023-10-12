#' Internal function to `compute()` for `type = "species"`
#' @noRd
#' @keywords Internal
compute_species <- function(.data){
  if(is_gbif()){
    result <- compute_occurrences(.data)
    result$type <- "data/species"
    result
  }else{
    class(.data) <- "query"
    return(.data)
  }
}