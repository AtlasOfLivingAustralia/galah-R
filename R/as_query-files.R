#' Internal version of `as_query()` for `request_files(type = "media")`
#' @param .query An object of class `files_request` (from `request_files()`)
#' @noRd
#' @keywords Internal
as_query_media_files <- function(.query, 
                                 thumbnail = FALSE,
                                 error_call = rlang::caller_env()
                                 ){
  # handle filters
  if(is.null(.query$filter)){
    cli::cli_abort("`collapse()` requires a `filter()` argument to function.",
                   call = error_call)
  }
  df <- .query$filter
  if(any(colnames(df) == "media_id")){
    identifiers <- df$media_id
  }else if(any(colnames(df) == "image_id")){
    identifiers <- df$image_id
  }else{
    cli::cli_abort("No valid identifiers found in supplied data.",
                   call = error_call)
  }
  path <- build_file_path(ids = identifiers, types = df$mimetype)
  if(any(colnames(df) == "image_url")){
    url <- df$image_url
  }else{
    url <- url_lookup("files/images", 
                      id = identifiers)
  }
  # handle thumbnails
  if(thumbnail){
    url <- gsub("/original", "/thumbnail", url)
  }
  
  # create result
  result <- list(
    type = "files/media",
    url = tibble::tibble(url = url, path = path),
    headers = build_headers())
  
  class(result) <- "query"
  return(result)
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