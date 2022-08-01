#' Collect media
#'
#' This function downloads images using information from `atlas_media`
#'
#' @param df tibble returned by `atlas_media()` or `show_all_media()`
#' @param type `string`: either `"full"` to download original images, or 
#' `"thumbnail"` to download thumbnails
#' @param path `string`: path to directory where downloaded media will
#' be stored
#' @param download_dir DEPRECATED: use `path` instead
#' @param refresh_cache `logical`: if set to `TRUE` and 
#' `galah_config(caching = TRUE)` then files cached from a previous query will 
#' be replaced by the current query. NOTE: unclear that this works right now.
#'
#' @export collect_media

collect_media <- function(df, type = c("full", "thumbnail"), path,
download_dir, refresh_cache
){

  # check inputs
  type <- match.arg(type)
  caching <- getOption("galah_config")$caching
  verbose <- getOption("galah_config")$verbose
  assert_that(!missing(path) | !missing(download_dir),
    msg = "A path to an existing directory to download images to is required")
  if(missing(path)){
    inform("The argument `download_dir` is deprecated, use `path` instead")
    path <- download_dir
  }
  if(is.null(path)){
    abort("argument `path` is missing, with no default")
  }else{
    if(!file.exists(path)){
      abort("argument `path` is missing, with no default")
    }
  }
  download_dir <- normalizePath(path)
  
  # remove rows with no information
  assert_that(all(c("mime_type", "media_id") %in% colnames(df)))  
  df <- df[apply(
    df[, c("mime_type", "media_id")], 1, 
    function(a){all(!is.na(a))}), ]

  # set up download
  df$url <- media_urls(df$media_id, 
                     is_image = (df$mime_type == "image/jpeg"),
                     thumbnail = (type == "thumbnail"))
  df$download_path <- media_outfiles(df$media_id, 
                                     df$mime_type, 
                                     path)
   
  # download images
  if (verbose) {
    n_files <- nrow(df)
    # NOTE: the blank space tells glue to add a leading newline before message
    inform(glue("
                
                Downloading {n_files} media files..."))
  }
  download_ok <- download_media(
    df = df[, c("url", "download_path")], 
    verbose)
  if(is.null(download_ok)){
    bullets <- c(
      "Calling the API failed for `atlas_species`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct.",
      i = "If you continue to see this message, please email support@ala.org.au."
    )
    inform(bullets)
    # return(df)
  }
  # NOTE: This only gets triggered if the image service is down,
  # but the biocache and metadata services are still working

  if (verbose) {
    n_files <- nrow(df)
    # NOTE: Do not delete blank space
    inform(glue("
                
                
                {n_files} files were downloaded to {download_dir}"))
  }
  return(df)
}


download_media <- function(df, verbose) {
  if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }
  n <- seq_len(nrow(df))
  x <- split(df, n)
  results <- lapply(n,
    function(a){      
      cli <- HttpClient$new(
        url = x[[a]]$url,
        headers = list(
          "User-Agent" = galah_version_string()
        )
      )
      res <- cli$get(disk = x[[a]]$download_path)
      if (verbose) {
        val <- (a / max(n))
        setTxtProgressBar(pb, val)
      }
      res
    }
  )
  # if all calls failed, return NULL
  status_failed <- unlist(lapply(results, function(a){a$status_code})) == 0
  if(all(status_failed)){
    return(NULL)
  }else{
    return("OK")
  }
}

# ## OLD CODE: fails at `cc$get(disk = outfiles[start:end])` for unknown reasons
# # Download images in batches of 124. The limit is due to a max on the
# # number of concurrently open connections.
# # The asynchronous method is slightly quicker than downloading all
# # images in a loop
# # BUT this means that files are created even if they are empty
# # Ergo we need a process for detecting failed calls and deleting the 
# # associated 'images'
# download_media_async <- function(urls, outfiles, verbose) {
#   if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }
#   calls <- ceiling(length(urls) / 124)
#   if(calls > 1){
#     results <- lapply(seq_len(calls - 1), function(x) {
#       start <- 1 + (x - 1) * 124
#       end <- x * 124
#       cc <- Async$new(urls = urls[start:end])
#       res <- cc$get(disk = outfiles[start:end])
#       status_failed <- unlist(lapply(res, function(a){a$status_code})) == 0
#       if(any(status_failed)){
#         unlink(outfiles[start:end][which(status_failed)])
#       }    
#       if (verbose) {
#         val <- (end / length(urls))
#         setTxtProgressBar(pb, val)
#       }
#       status_failed
#     })
#   }
# 
#   # TODO: Extract this part into to a function like ala_async_get()
#   # Download remaining images
#   start <- 1 + (calls - 1) * 124
#   end <- length(urls)
#   cc <- Async$new(
#     urls = c(urls[start:end]), 
#     headers = list(
#       "User-Agent" = galah_version_string()
#     )
#   )
#   res <- cc$get(disk = outfiles)#[start:end])
#   status_failed <- unlist(lapply(res, function(a){a$status_code})) == 0
#   if(any(status_failed)){
#     unlink(outfiles[start:end][which(status_failed)])
#   } 
#   results[[calls]] <- status_failed
#   if (verbose) {
#     val <- (end / length(urls))
#     setTxtProgressBar(pb, val)
#   }
# 
#   # if all calls failed, return NULL
#   if(all(do.call(c, results))){
#     return(NULL)
#   }else{
#     return("OK")
#   }
# }

# Construct url paths to where media will be downloaded from
# Returns a vector of urls; one per id
media_urls <- function(ids, is_image, thumbnail = TRUE) {
  url <- parse_url(server_config("images_base_url"))
  # if(thumbnail){
  #   end_text <- "thumbnail"
  # }else{
  #   end_text <- "original"
  # }
  unlist(lapply(seq_len(length(ids)), function(x) {
    url$path <- c(
      "image", 
      as.character(ids[x]), 
      (if(thumbnail & is_image[x]){"thumbnail"}else{"original"})
      )
    # may be quicker to use `paste` here?
    build_url(url)
  }))
}

# Construct paths to where media will be downloaded
# Returns a vector of paths; one per id
media_outfiles <- function(ids, types, download_dir) {
  unlist(lapply(seq_len(length(ids)), function(x) {
    ext <- switch(types[x],
                  "image/jpeg" = ".jpg",
                  "image/png" = ".png",
                  "audio/mpeg" = ".mpg",
                  "audio/x-wav" = ".wav",
                  "audio/mp4" = ".mp4",
                  "image/gif" = ".gif",
                  "video/3gpp" = ".3gp",
                  "video/quicktime" = ".mov",
                  "audio/vnd.wave" = ".wav",
                  ""
    )
    file.path(download_dir, paste0(ids[x], ext))
  }))
}