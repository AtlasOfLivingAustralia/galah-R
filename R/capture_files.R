#' Internal version of `capture()` for `request_files(type = "media")`
#' @param .query An object of class `files_request` (from `request_files()`)
#' @noRd
#' @keywords Internal
capture_media_files <- function(.query, 
                                 thumbnail = FALSE,
                                 error_call = rlang::caller_env()
                                 ){
  # handle filters
  if(is.null(.query$filter)){
    cli::cli_abort("`This function requires a `filter()` argument",
                   call = error_call)
  }else if(!inherits(.query$filter, "files_filter")){
    cli::cli_abort("Downloading media files requires a `filter()` query to work",
                   call = error_call)
  }
  df <- .query$filter$data
  
  # find unique identifiers anywhere you can
  if(any(colnames(df) == "media_id")){
    identifiers <- df$media_id
  }else if(any(colnames(df) == "image_id")){
    identifiers <- df$image_id
  }else{
    cli::cli_abort("No valid identifiers found in supplied data.",
                   call = error_call)
  }

  # create output
  list(type = "files/media",
       url = build_media_tibble(identifiers,
                                df$mime_type,
                                thumbnail),
       headers = build_headers()) |>
    as_query()
  
}

#' Internal function to create API urls AND file paths
#' @noRd
#' @keywords Internal
build_media_tibble <- function(identifiers,
                               mimetype,
                               thumbnail = FALSE){
  # create a `size` vector
  # this is complicated because thumbnails fail for sounds
  size_vec <- rep(
    ifelse(thumbnail, "thumbnail", "original"),
    length(identifiers))
  image_lookup <- mimetype %in% c("image/jpg", "image/jpeg", "image/png")
  if(any(!image_lookup) & isTRUE(thumbnail)){
    size_vec[!image_lookup] <- "original"
  }
  
  # return a result
  tibble::tibble(
    url = url_lookup("files/media",
                     id = identifiers,
                     size = size_vec),
    path = build_file_path(ids = identifiers,
                           types = mimetype) 
  )
}

#' build file paths that include 1. path, 2. file name, 3. correct extension
#' @noRd
#' @keywords Internal
build_file_path <- function(ids, types){
  path <- potions::pour("package", "directory", .pkg = "galah")
  ext <- dplyr::case_match(types,
                           "image/jpg" ~ "jpg",
                           "image/jpeg" ~ "jpg",
                           "image/png" ~ "png",
                           "audio/mpeg" ~ "mpg",
                           "audio/x-wav" ~ "wav",
                           "audio/mp4" ~ "mp4",
                           "image/gif" ~ "gif",
                           "video/3gpp" ~ "3gp",
                           "video/quicktime" ~ "mov",
                           "audio/vnd.wave" ~ "wav")  
  glue::glue("{path}/{ids}.{ext}") |> 
    as.character()
}

