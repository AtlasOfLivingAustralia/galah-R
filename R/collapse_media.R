#' Internal version of `collapse()` for `request_data(type = "media")`
#' @param .data An object of class `data_request` (from `request_data()`)
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_media_metadata <- function(.data){
  
  if(is.null(.data$filter)){
    abort("`collapse()` requires a `filter()` argument to function")
  }
  
  if(is.null(.data$filter$media_id)){
   abort("specified filters do not contain the required `media_id` column") 
  }
  
  result <- list(
    type = .data$type,
    url = url_lookup("image_metadata", id = .data$filter$media_id),
    headers = build_headers())
  class(result) <- "data_query"
  return(result)
}

#' Internal version of `collapse()` for `request_files(type = "media")`
#' @param .data An object of class `files_request` (from `request_files()`)
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_media_files <- function(.data){
  
  if(is.null(.data$filter)){
    abort("`collapse()` requires a `filter()` argument to function")
  }
  
  if(is.null(.data$filter$media_url)){
    abort("specified filters do not contain the required `media_urls` column") 
  }
  
  result <- list(
    type = .data$type,
    url = .data$filter$media_url, # this is a guess
    headers = build_headers())
  class(result) <- "data_query"
  return(result)
}