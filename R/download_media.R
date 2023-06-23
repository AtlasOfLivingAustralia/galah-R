#' Download media files
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
#'   download_media(path = here::here("folder", "subfolder"))
#' }
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom assertthat assert_that
#' @noRd
#' @keywords Internal
download_media <- function(.data,  # change .data to object of class `tibble`
                           filesize = "full", 
                           path = "."){
  
  ## NOTE: bring in testing code from `galah_media()` here
  
  .data <- check_media_args(.data, filesize = filesize, path = path)
  
  if(pour("package", "verbose")){
    inform(glue("Downloading {.data$summary$n} media files with total size {.data$summary$size}"))
  }
  # NOTE: filesize is incorrect if filesize = "thumbnail"
  
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
  download_ok <- download_media_internal(.data$data[, c("image_url", "image_file")])

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
download_media_internal <- function(df) {
  
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


#' Internal function to get media data
#' @param .data An object of class `data_request` (from `galah_call()`)
#' @importFrom dplyr filter
#' @noRd
#' @keywords Internal
OLD_collapse_media <- function(.data){
  
  # overwrite earlier `select` args to only allow minimal fields
  lower_types <- paste0(tolower(.data$type), "s")
  
  # overwrite `select` functions to only return required information
  # NOTE: order matters here; if recordID is not first, no data are returned
  .data$select <- galah_select(
    "recordID", 
    # group = "media") |>
    group = c("basic", "media")) |>
    filter(name %in% c("recordID", "multimedia", lower_types))
  
  # pass to collapse_occurrences for query construction
  result <- collapse_occurrences(.data)
  
  # filter to records that contain media of requested types 
  # NOTE: Might be more efficient to use `filter` for this, as it 
  # includes to remove duplicated rows
  media_fq <- glue("({lower_types}:*)") |>
    glue_collapse(" OR ")
  result$query$fq <- glue_collapse(c(result$query$fq, media_fq), " AND ") |>
    as.character()
  
  # convert to correct type and return
  result$type <- "media"
  return(result)
}

#' Compute media
#' 
#' Note that this is effectively synonymous with 
#' collect("occurrences", type = "media").
#' 
#' @param .data An object of class `data_query` (from `collapse.data_request()`)
#' @noRd
#' @keywords Internal
OLD_compute_media <- function(.data, type){
  # get occurrences with associated metadata
  .data$type <- "occurrences"
  resp <- collect(.data) |>
    collect_occurrences_media() # NOTE: update this to join()
  
  # error catching
  if(nrow(resp) < 1){
    abort("No data returned by compute('media')")
  }
  
  # convert to a list-like format
  result <- list(
    summary = list(
      n = nrow(resp),
      size = format_bytes(sum(resp$size_in_bytes))),
    data = resp[, c("media_id", "file_extension", "image_url", "size_in_bytes")],
    type = "media")
  class(result) <- "data_response"
  result
}

#' Internal function for formatting file sizes
#' @param x integer number of bytes
#' @noRd
#' @keywords Internal
format_bytes <- function(x){
  x_units <- cut(log10(x), 
                 breaks = c(0, 3, 6, 9, 12, Inf), 
                 labels = c("B", "KB", "MB", "GB", "TB")) |>
    as.character()
  x_scaled <- switch(x_units,
                     "B" = x,
                     "KB" = x * 10^-3,
                     "MB" = x * 10^-6,
                     "GB" = x * 10^-9,
                     "TB" = x * 10^-12)
  paste(
    round(x_scaled, digits = 2),
    x_units,
    sep = " ")
}