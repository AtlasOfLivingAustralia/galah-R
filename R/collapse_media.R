#' Internal version of `collapse()` for `request_data(type = "media")`
#' @param .data An object of class `data_request` (from `request_data()`)
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_media <- function(.data){
  result <- list(
    type = "data/media",
    url = url_lookup("data/media"),
    body = list(), # this gets populated during `compute()`
    headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal version of `collapse()` for `request_files(type = "media")`
#' @param .data An object of class `files_request` (from `request_files()`)
#' @importFrom rlang abort
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
collapse_media_files <- function(.data, 
                                 thumbnail = FALSE
                                 ){
  # ensure filter is supplied
  if(is.null(.data$filter)){
    abort("`collapse()` requires a `filter()` argument to function")
  }
  # add checking to ensure correct columns are present in `filter`
  # handle thumbnails
  if(thumbnail){
    urls <- gsub("/original$", "/thumbnail", .data$filter$image_url)
  }else{
    urls <- .data$filter$image_url
  }
  # construct data.frame
  # NOTE: this intermediate step is needed because `tibble()` doesn't recognise
  # use of `$` to search through a `list` by name
  result_df <- data.frame(url = urls, 
                          path = build_file_path(.data))
  # create result
  result <- list(
    type = .data$type,
    url = tibble(result_df),
    headers = build_headers())
  class(result) <- "data_query"
  return(result)
}

#' build file paths that include 1. path, 2. file name, 3. correct extension
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
build_file_path <- function(.data){
  path <- pour("package", "directory", .pkg = "galah")
  ids <- .data$filter$media_id
  ext <- build_file_extension(.data$filter$mime_type)
  glue("{path}/{ids}.{ext}") |> as.character()
}

#' get extensions for media files
#' @importFrom dplyr case_match
#' @noRd
#' @keywords Internal
build_file_extension <- function(x){
  case_match(x,
             "image/jpeg" ~ "jpg",
             "image/png" ~ "png",
             "audio/mpeg" ~ "mpg",
             "audio/x-wav" ~ "wav",
             "audio/mp4" ~ "mp4",
             "image/gif" ~ "gif",
             "video/3gpp" ~ "3gp",
             "video/quicktime" ~ "mov",
             "audio/vnd.wave" ~ "wav")  
}