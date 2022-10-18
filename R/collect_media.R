#' Collect media files
#'
#' This function downloads full-sized or thumbnail images and media files using 
#' information from `atlas_media` to a local directory.
#'
#' @param df tibble returned by `atlas_media()` or `show_all(media)`
#' @param type `string`: either `"full"` to download original images, or 
#' `"thumbnail"` to download thumbnails
#' @param path `string`: path to directory where downloaded media will
#' be stored
#' @param download_dir 
#'    `r lifecycle::badge("deprecated")` Use `path` instead.
#' @param refresh_cache `logical`: if set to `TRUE` and 
#' `galah_config(caching = TRUE)` then files cached from a previous query will 
#' be replaced by the current query. NOTE: Unclear that this works right now.
#'
#' @return Available image & media files downloaded to a user local directory.
#'
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Use `atlas_media()` to return a `tibble` of records that contain media
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_call() |> 
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2015) |>
#'   atlas_media()
#' ```
#' 
#' Then add `collect_media()` to the end of a query to download media files
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_call() |> 
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2015) |>
#'   atlas_media() |>
#'   collect_media(path = here::here("folder", "subfolder"))
#' ```
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
      abort("The specified `path` does not exist")
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
    system_down_message("collect_media")
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


# Construct url paths to where media will be downloaded from
# Returns a vector of urls; one per id
media_urls <- function(ids, is_image, thumbnail = TRUE) {
  url <- atlas_url("image_metadata") |>
         parse_url()
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