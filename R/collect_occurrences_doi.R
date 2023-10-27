#' Subset of collect for a doi.
#' @param q_obj An object of class `data_request`
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
collect_occurrences_doi <- function(q_obj, file = NULL, error_call = caller_env()) {

  q_obj$file <- check_download_filename(file)
  query_API(q_obj)
  result <- load_zip(q_obj$file)
  
  if(is.null(result)){
    inform("Download failed.")
    return(tibble())
  }else{
    attr(result, "doi") <- q_obj$doi
    return(result)
  }
}
