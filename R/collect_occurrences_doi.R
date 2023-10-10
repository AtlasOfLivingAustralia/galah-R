#' Subset of collect for a doi.
#' @param .data An object of class `data_request`
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
collect_occurrences_doi <- function(.data, file = NULL, error_call = caller_env()) {

  .data$file <- check_download_filename(file)
  query_API(.data)
  result <- load_zip(.data$file)
  
  if(is.null(result)){
    inform("Download failed.")
    return(tibble())
  }else{
    attr(result, "doi") <- .data$doi
    return(result)
  }
}
