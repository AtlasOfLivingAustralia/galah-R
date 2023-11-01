#' Internal function to `compute()` for `type = "species"`
#' @noRd
#' @keywords Internal
compute_species <- function(.query){
  if(is_gbif()){
    result <- compute_occurrences(.query)
    result$type <- "data/species"
    result
  }else{
    class(.query) <- "query"
    return(.query)
  }
}
