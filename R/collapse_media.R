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
  # handle filters
  if(is.null(.data$filter)){
    abort("`collapse()` requires a `filter()` argument to function")
  }
  df <- .data$filter
  id <- build_media_id(df)
  path <- build_file_path(ids = id, types = df$mimetype)
  if(any(colnames(df) == "image_url")){
    url <- df$image_url
  }else{
    url <- url_lookup("files/images", id = id)
  }
  result <- tibble(url = url, path = path)
  # handle thumbnails
  if(thumbnail){
    urls <- gsub("/original", "/thumbnail", urls)
  }
  # create result
  result <- list(
    type = "files/media",
    url = result,
    headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to get media metadata, and create a valid file name
#' @noRd
#' @keywords Internal
build_media_id <- function(df){
  # create a column that includes media identifiers, regardless of which column they are in
  ## NOTE: I haven't found good tidyverse syntax for this yet
  x <- rep(NA, nrow(df))
  videos <- !is.na(df$videos)
  if(any(videos)){x[videos] <- df$videos[videos]}
  sounds <- !is.na(df$sounds)
  if(any(sounds)){x[sounds] <- df$sounds[sounds]}
  images <- !is.na(df$images)
  if(any(images)){x[images] <- df$images[images]}
  x
}

#' build file paths that include 1. path, 2. file name, 3. correct extension
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
build_file_path <- function(ids, types){
  path <- pour("package", "directory", .pkg = "galah")
  ext <- build_file_extension(types)
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