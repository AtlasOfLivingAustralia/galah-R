#' Internal function to `compute()` for `type = "species"`
#' @noRd
#' @keywords Internal
compute_species <- function(q_obj){
  if(is_gbif()){
    result <- compute_occurrences(q_obj)
    result$type <- "data/species"
    result
  }else{
    class(q_obj) <- "query"
    return(q_obj)
  }
}
