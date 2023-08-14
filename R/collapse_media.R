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

  # construct data.frame
  # NOTE: this intermediate step is needed because `tibble()` doesn't recognise
  # use of `$` to search through a list by name
  result_df <- data.frame(
    # make file name
    # NOTE: doesn't include path from `pour()` yet
    file_name = paste0(.data$filter$media_id, 
                       build_file_extension(.data$filter$mime_type)),
    # where is this file?
    # NOTE: doesn't account for `thumbnail` option yet
    url = .data$filter$image_url)
  
  # create result object
  result <- list(
    type = .data$type,
    url = tibble(result_df),
    headers = build_headers())
  class(result) <- "data_query"
  return(result)
}

#' get extensions for media files
#' @importFrom dplyr case_match
#' @noRd
#' @keywords Internal
build_file_extension <- function(x){
  case_match(x,
             "image/jpeg" ~ ".jpg",
             "image/png" ~ ".png",
             "audio/mpeg" ~ ".mpg",
             "audio/x-wav" ~ ".wav",
             "audio/mp4" ~ ".mp4",
             "image/gif" ~ ".gif",
             "video/3gpp" ~ ".3gp",
             "video/quicktime" ~ ".mov",
             "audio/vnd.wave" ~ ".wav")  
}
