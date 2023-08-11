#' Internal version of `collapse()` for `type = "media"`
#' @param .data An object of class `data_request` (from `galah_call()`)
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