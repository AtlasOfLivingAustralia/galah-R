#' Internal function to get media metadata
#' @param .data An object of class `data_request`
#' @noRd
#' @keywords Internal
collapse_media_metadata <- function(.data){
  
  # first a use case where galah_call() is passed with filters etc.
  if(is.null(.data$ids)){
    .data$select <- galah_select(
      "recordID", 
      group = c("basic", "media")) |>
      filter(name %in% c("recordID", "multimedia", .data$media$media))
    .data$type <- "occurrences"
    result <- collapse_occurrences(.data)
    result$type <- "media-metadata"
  # alternatively where someone has done these steps already
  }else{
    # convert to a vector with no gaps
    ids <- unlist(.data$ids)  
    names(ids) <- NULL
    # create output object
    result <- list(url = sub("%7Bid%7D", "", url_lookup("image_metadata")),
                   ids = ids[!is.na(ids) & ids != ""],
                   type = .data$type)
    class(result) <- "data_query"
  }
  return(result)
}