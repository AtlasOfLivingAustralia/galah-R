#' Function to specify how media should be handled by `galah`
#'
#' In prep
#' @param .data An object of class `data_request`
#' @param path String: where should data be saved?
#' @param filesize String: either "full" (default) or "thumbnail". Note only
#' applies if `media = "image"`.
#' @param media Vector: One or more of "image", "sound" or "video", defaulting
#' to all three.
#' @export
galah_media <- function(.data,
                        path = ".", 
                        filesize = c("full", "thumbnail"), 
                        media = c("image", "sound", "video")){
  if(!all(media %in% c("image", "sound", "video"))){
    abort("media must only include the values 'image', 'sound' or 'video'")
  }
  
  if(is.null(path)){
    # cached_path <- pour("package", "cache_directory") # add later
    abort("argument `path` is missing, with no default")
  }else{
    if(!file.exists(path)){
      abort("The specified `path` does not exist")
    }
  }

  filesize <- match.arg(filesize)
  if(!is.character(filesize)){
    abort("argument `filesize` must be of class `character`")
  }
  if(!(filesize %in% c("full", "thumbnail"))){
    abort("`type` must be one of either 'full' or 'thumbnail'")
  }
  
  result <- list(
    path = normalizePath(path),
    filesize = filesize,
    media = media)
 
  if(missing(.data)){
    return(result)
  }else{
    update_galah_call(.data, media = result)
  } 
}

#' @rdname galah_media
#' @export
media <- function(.data, path, filesize, media){
  galah_media(.data, path, filesize, media)
}