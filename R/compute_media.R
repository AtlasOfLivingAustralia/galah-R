#' Compute media
#' 
#' Note that this is effectively synonymous with 
#' collect("occurrences", type = "media").
#' 
#' @params .data An object of class `data_query` (from `collapse.data_request()`)
#' @noRd
#' @keywords Internal
compute_media <- function(.data, type){
  .data$what <- "occurrences"
  resp <- collect(.data)
  
  if(nrow(resp) < 1){
    abort("No data returned by compute('media')")
  }
  
  # parse out media identifiers
  ids <- do.call(c, resp[, colnames(resp) %in% c("images", "sounds", "videos")])
  single_ids <- do.call(c, str_split(ids, pattern = "\\s\\|\\s"))
  
  # convert to a list-like format
  result <- list(
    data = single_ids[!is.na(single_ids)],
    what = "media")
  class(result) <- "data_response"
  result
}

