#' Image and sounds
#'
#' In addition to text data describing individual occurrences and their attributes,
#' ALA stores image and sounds associated with a given record. These can be
#' downloaded to \code{R} by first using \code{\link{ala_occurrences}()} to
#' find records that contain media, and then passing the resulting \code{recordID}
#' field to \code{\link{ala_media}()} to download those media files (see examples).
#'
#' @param identifier \code{string}: a single or vector of occurrence or media
#' identifiers. The type is specified by \code{identifier_type}
#' @param identifier_type \code{string}: one of \code{c("occurrence", "media")}.
#' Defaults to \code{"media"}.
#' @param download_dir \code{string}: path to directory to store the downloaded media
#' in
#' @param media_type \code{string}: type of media to download, one or both of
#' \code{c("image", "sound")}. Defaults to both.
#' @return \code{data.frame} of media information
#' @examples
#' \dontrun{
#' # Search for galah records with images
#' occ <- ala_occurrences(
#'     taxa = select_taxa("Eolophus Roseicapilla"),
#'     filters = select_filters(multimedia = "Image", year = 2020))
#'
#' # get images associated with these records
#' images <- ala_media(occ$recordID,
#'     identifier_type = "occurrence",
#'     download_dir = tempdir(), media_type = "image")
#' }
#' @export ala_media

ala_media <- function(identifier, identifier_type = "media", download_dir,
                      media_type = c("image", "sound")) {
  config_verbose <- getOption("galah_config")$verbose
  assert_that(!missing(identifier),
              msg = "Please provide at least one identifier")
  assert_that(is.character(identifier))

  valid_media_type <- media_type %in% c("image", "sound")
  if (!all(valid_media_type)) {
    stop("Valid media types are `c('image', 'sound')`")
  }
  assert_that(
    identifier_type %in% c("occurrence", "media"),
    msg = "`identifier_type` must be one of `c('occurrence', 'media')`")


  assert_that(!missing(download_dir), msg = "Directory to download media to is
              required")
  assert_that(file.exists(download_dir))

  media_data <- data.table::rbindlist(lapply(identifier, function(id) {
    if (identifier_type == "media") {
      # get the media specified
      data <- cbind(media_id = id, as.data.frame(media(id),
                                                 stringsAsFactors = FALSE))
    } else {
      # get media for an occurrence id
      data <- occurrence_media(id, media_type)
    }
    data
  }), fill = TRUE)

  # download media
  # should add output path to out data?
  d <- mapply(download_media, media_data$media_id, media_data$format,
              download_dir)
  if (config_verbose) {
    message(nrow(media_data), " files were downloaded to ", download_dir)
  }
  media_data
}

# should also allow filters?
# handle the case when an occurrence record has more images than the limit?
# is there a limit?
occurrence_media <- function(occurrence_id, file_type) {
  query <- list(q = paste0("occurrenceID:", "\"", occurrence_id, "\""))
  if (length(file_type) == 1) {
    query$fq <- paste0("fileType:", file_type)
  }
  url <- getOption("galah_server_config")$base_url_images
  resp <- ala_GET(url, "ws/search", params = query
                  )

  if(resp$totalImageCount == 0) {
    warning("No media was found for id ", "\"", occurrence_id, "\"")
    return(NULL)
  }
  resp <- resp$images
  names(resp) <- rename_columns(names(resp), type = "media")
  resp[names(resp) %in% wanted_columns("media")]
}

# what to do if no media found? create error for now
media <- function(media_id) {
  url <- getOption("galah_server_config")$base_url_images
  tryCatch(
    resp <- ala_GET(url, paste0("ws/image/", media_id)),
    error = function(e) {
      e$message <- paste0("No media found for id ", "\"", media_id, "\"")
      stop(e)
    }
  )
  # workaround for recognised licence returning null
  if (is.null(resp$recognisedLicence)) {
    resp$recognisedLicence <- NA
  }
  names(resp) <- rename_columns(names(resp), type = "media")
  resp[names(resp) %in% wanted_columns("media")]
}

download_media <- function(id, type, download_dir) {
  url <- parse_url(getOption("galah_server_config")$base_url_images)
  url$path <- c("image", as.character(id), "original")
  ext <- switch (type,
    "image/jpeg" = ".jpg",
    "image/png" = ".png",
    "audio/mpeg" = ".mpg",
    "audio/x-wav" = ".wav",
    "audio/mp4" = ".mp4",
    "audio/vnd.wave" = ".wav"
  )
  out_path <- file.path(download_dir, paste0(id, ext))
  download.file(build_url(url), destfile = out_path, quiet = TRUE)
  # throttle download requests
  Sys.sleep(1)
}
