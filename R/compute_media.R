#' Internal version of `compute()` for `type = "media"`
#' @param .data An object of class `data_query` (from `collapse()`)
#' @importFrom dplyr filter
#' @noRd
#' @keywords Internal
compute_media <- function(.data){
  .data$type <- "occurrences"
  result <- compute(.data)
  result$type <- "media"
  return(result)
}