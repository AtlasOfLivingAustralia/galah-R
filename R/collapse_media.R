#' Internal version of `collapse()` for `request_metadata(type = "media")`
#' @param .data An object of class `metadata_request` (from `request_metadata()`)
#' @importFrom jsonlite toJSON
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_media <- function(.data){
  # NOTE:
  # this function currently assumes that the user has passed an occurrence 
  # tibble verbatim to filter, i.e.
  # `request_metadata() |> filter(media = occurrences) |> collapse()`
  # It may be useful to support passing of media_ids directly, e.g.
  # `request_metadata() |> filter(media = occurrences$images`) |> collapse()
  if(is.null(.data$filter)){
    abort("requests for `metadata` with type `media` require information passed via `filter()`")
  }
  occ <- .data$filter
  media_cols <- which(colnames(occ) %in% c("images", "videos", "sounds"))
  media_ids <- do.call(c, occ[, media_cols]) |>
    unlist()
  media_ids <- media_ids[!is.na(media_ids)]
  names(media_ids) <- NULL
  result <- list(
    type = "metadata/media",
    url = url_lookup("metadata/media"),
    body = toJSON(list(imageIds = media_ids)),
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
  if(any(colnames(df) == "media_id")){
    identifiers <- df$media_id
  }else if(any(colnames(df) == "image_id")){
    identifiers <- df$image_id
  }else{
    abort("No valid identifiers found in supplied data.")
  }
  path <- build_file_path(ids = identifiers, types = df$mimetype)
  if(any(colnames(df) == "image_url")){
    url <- df$image_url
  }else{
    url <- url_lookup("files/images", id = identifiers)
  }
  # handle thumbnails
  if(thumbnail){
    url <- gsub("/original", "/thumbnail", url)
  }
  # create result
  result <- list(
    type = "files/media",
    url = tibble(url = url, path = path),
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