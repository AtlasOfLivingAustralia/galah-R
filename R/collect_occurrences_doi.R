#' Subset of collect for a doi.
#' @param .query An object of class `data_request`
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
collect_occurrences_doi <- function(.query, file = NULL, error_call = caller_env()) {

  .query$file <- check_download_filename(file)
  query_API(.query)
  result <- load_zip(.query$file)
  
  if(is.null(result)){
    inform("Download failed.")
    return(tibble())
  }else{
    attr(result, "doi") <- .query$doi
    return(result)
  }
}
