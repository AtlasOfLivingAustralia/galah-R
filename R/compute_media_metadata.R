#' Internal function to get media metadata
#' @param .data An object of class `data_query`
#' @noRd
#' @keywords Internal
compute_media_metadata <- function(.data){
  # where occurrences need to be downloaded before IDs can be retrieved
  if(is.null(.data$ids)){
    .data$type <- "occurrences"
    occurrences <- collect(.data)
    result <- galah_call(type = "media-metadata",
                         ids = do.call(c, occurrences[.data$media$media])) |>
      collapse()
    
  # or, if user has done this work for us, just pass the object through
  }else{
    result <- .data
  }
  class(result) <- "data_response"
  return(result)
}