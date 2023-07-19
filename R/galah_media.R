#' Function to specify how media should be handled by `galah`
#'
#' In prep
#' @param ... Optional additional arguments, typically including an object of 
#' class `data_request`.
#' @param id Optional vector giving media identifiers to be collected.
#' @param media Vector: One or more of "image", "sound" or "video", defaulting
#' to all three. Ignored if `id` is specified.
#' @param path String: where should data be saved?
#' @param filesize String: either "full" (default) or "thumbnail". Note only
#' applies if `media = "image"`.
#' @param .join String: if `type = "occurrences"`, how should occurrences and 
#' media metadata be joined? Should be one of the `dplyr` mutating joins, 
#' defaulting to `"full_join"`.
#' @export
galah_media <- function(...,
                        id = NULL,
                        format = c("image", "sound", "video"),
                        path = ".", 
                        filesize = c("full", "thumbnail"), 
                        .join = c("full_join", "inner_join", "left_join", "right_join")
                        ){
  inputs <- list(...)
  if(is.null(inputs)){
    data_request <- NULL
  }else{
    dr_check <- lapply(inputs, 
                       function(a){inherits(a, "data_request")}) |>
                unlist()
    if(any(dr_check)){
      data_request <- inputs[[which(dr_check)[[1]]]]
    }else{
      data_request <- NULL
    }
  }
  
  if(!all(format %in% c("image", "sound", "video"))){
    abort("'format' must only include the values 'image', 'sound' or 'video'")
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
    .join = match.arg(.join))
  
  if(is.null(id)){
    result <- c(list(format = format), result)
  }else{
    result <- c(list(id = id), result)
  }
 
  if(is.null(data_request)){
    return(result)
  }else{
    update_data_request(data_request, media = result)
  } 
}

#' @rdname galah_media
#' @export
media <- galah_media
