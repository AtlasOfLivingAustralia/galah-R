#' Collect media files
#'
#' This function downloads full-sized or thumbnail images and media files using 
#' information from `atlas_media` to a local directory.
#'
#' @param .data object of class `data_response`, calculated with `compute()`
#' @return Available image & media files downloaded to a user local directory.
#'
#' @examples \dontrun{
#' # Use `atlas_media()` to return a `tibble` of records that contain media
#' galah_call() |> 
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2015) |>
#'   atlas_media()
#' 
#' # To download media files, add `collect_media()` to the end of a query 
#' galah_call() |> 
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2015) |>
#'   atlas_media() |>
#'   collect_media(path = here::here("folder", "subfolder"))
#' }
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom assertthat assert_that
#' @noRd
#' @keywords Internal
collect_media <- function(.data, filesize = "full", path = "."){
  
  .data <- check_media_args(.data, filesize = filesize, path = path)
  
  if(pour("package", "verbose")){
    inform(glue("Downloading {.data$summary$n} media files with total size {.data$summary$size}"))
  }
  # NOTE: filesize is incorrect is filesize = "thumbnail"
  
  # set fullsize/not
  if(.data$filesize != "full"){
    .data$data$image_url <- str_replace(.data$data$image_url, 
                                        "/original$", 
                                        "/thumbnail")
  }
  
  # set final location for each file
  .data$data$image_file <- paste0(
    .data$path,
    "/",
    .data$data$media_id,
    .data$data$file_extension)
  
  # get images
  download_ok <- download_media(.data$data[, c("image_url", "image_file")])

  # respond to errors  
  if(is.null(download_ok)){
    system_down_message("collect_media")
  }
  # NOTE: This only gets triggered if the image service is down,
  # but the biocache and metadata services are still working

  if (pour("package", "verbose")) {
    inform("complete")
  }
}

#' Internal function to iteratively download all files
#' @noRd
#' @keywords Internal
download_media <- function(df) {
  
  verbose <- pour("package", "verbose")
  if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }
  
  n <- seq_len(nrow(df))
  results <- lapply(n,
    function(a){      
      cli <- HttpClient$new(
        url = df$image_url[a],
        headers = list(
          "User-Agent" = galah_version_string()
        )
      )
      res <- cli$get(disk = df$image_file[a])
      if (verbose) {
        val <- (a / max(n))
        setTxtProgressBar(pb, val)
      }
      res
    })
  
  # if all calls failed, return NULL
  status_failed <- unlist(lapply(results, function(a){a$status_code})) == 0
  if(all(status_failed)){
    return(NULL)
  }else{
    return("OK")
  }
}