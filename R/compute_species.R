#' Internal function to `compute()` for `type = "species"`
#' @noRd
#' @keywords Internal
compute_species <- function(.data){
  if(is_gbif()){
    compute_occurrences(.data)
  }else{
    class(.data) <- "data_response"
    return(.data)
  }
}