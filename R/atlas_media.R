#' Download images, sounds and videos
#'
#' In addition to text data describing individual occurrences and their 
#' attributes, ALA stores images, sounds and videos associated with a given 
#' record. \code{atlas_media} allows download of any and all of the media types. 
#'
#' @inheritParams atlas_occurrences
#' @param download_dir \code{string}: path to directory to store the downloaded
#' media in
#' @param refresh_cache \code{logical}: if set to `TRUE` and 
#' `galah_config(caching = TRUE)` then files cached from a previous query will 
#' be replaced by the current query
#' @details \code{\link{atlas_occurrences}()} works by first finding all occurrence records
#' matching the filter which contain media, then downloading the metadata for the
#' media and the media files. \code{\link{galah_filter}()} can take both filter
#' relating to occurrences (e.g. basis of records), and filter relating to media
#' (e.g. type of licence).
#' It may be beneficial when requesting a large number of records to show a progress
#' bar by setting \code{verbose = TRUE} in \code{\link{galah_config}()}.
#' @return \code{data.frame} of metadata of the downloaded media
#' @seealso \code{\link{atlas_counts}} to find the number of records with media- note this
#' is not necessarily the same as the number of media files, as each record can have
#' more than one media file associated with it (see examples section for how to do this).
#' @examples
#' \dontrun{
#' # Download Regent Honeyeater multimedia
#' media_data <- atlas_media(
#'     taxa = search_taxa("Regent Honeyeater"),
#'     filter = galah_filter(year == 2011),
#'     download_dir = "media")
#' 
#' # Specify a single media type to download
#' media_data <- atlas_media(
#'      taxa = search_taxa("Eolophus Roseicapilla"),
#'      filter = galah_filter(multimedia == "Sound"))
#' 
#' # Filter to only records with a particular licence type
#' media_data <- atlas_media(
#'       taxa = search_taxa("Ornithorhynchus anatinus"),
#'       filter = galah_filter(year == 2020,
#'       license = "http://creativecommons.org/licenses/by-nc/4.0/")
#' )
#' # Check how many records have media files
#' atlas_counts(
#'      filter = galah_filter(multimedia == c("Image","Sound","Video")),
#'      group_by = galah_group_by(multimedia)
#' )
#' }
#' @export
atlas_media <- function(taxa = NULL, 
                        filter = NULL, 
                        geolocate = NULL,
                        select = galah_select(group = "basic"),
                        columns = NULL, 
                        download_dir,
                        refresh_cache = FALSE) {

  image_url <- server_config("images_base_url")
  if(is.null(select) & !is.null(columns)){select <- columns}
  
  verbose <- getOption("galah_config")$verbose
  caching <- getOption("galah_config")$caching
  assert_that(!missing(download_dir),
  msg = "A path to an existing directory to download images to is required")
  assert_that(file.exists(download_dir))
  download_dir <- normalizePath(download_dir)
  
  if (getOption("galah_config")$atlas != "Australia") {
    stop("`atlas_media` only provides media from the ALA and cannot provide 
         media from international atlases.") #TODO: Update errors to glue
  }
  
  if (is.null(taxa) & is.null(filter) & is.null(geolocate)) {
    warning("No filter have been provided. All images and sounds will be downloaded.")
  }
  
  if (caching && !refresh_cache) {
    cache_file <- cache_filename(
      "media",
      unlist(build_query(taxa, filter, geolocate, select))
    )
    if (file.exists(cache_file)) {
      return(read_cache_file(cache_file))
    }
  }
  
  # Check whether any filters are media-specific filters and
  # filter to records with image/sound/video
  filter_available <- filter$variable %in% show_all_fields(type = "media")$id
  media_filter <- filter[filter_available, ]
  occ_filter <- rbind(
    filter[!(filter_available), ],
    galah_filter(multimedia = c("Image", "Sound", "Video")))
  
  # Make sure media ids are included in results
  occ_columns <- rbind(select, galah_select(image_select()))

  # add galah_ classes to modified filter and select
  class(occ_filter) <- append(class(occ_filter), "galah_filter")
  class(occ_columns) <- append(class(occ_columns), "galah_select")
  if (verbose) { message("Downloading records with media...") }
  
  occ <- atlas_occurrences(taxa, occ_filter, geolocate, occ_columns)

  # occurrence data.frame has one row per occurrence record and stores all media
  # ids in a single column; this code splits the media ids and creates one row
  # per media id in the returned data.frame
  occ_long <- data.frame(data.table::rbindlist(
    lapply(seq_len(nrow(occ)), function(x) {
      # get all the image, video and sound columns into one row
      splt_media <- unlist(str_split(occ[x,][image_select()],
                                     pattern = "\""))
      media <- splt_media[nchar(splt_media) > 1 & splt_media != "NA"]
      
      if (length(media) > 0) {
        rows <- occ[x,][rep(seq_len(nrow(occ[x,])), each = length(media)), ]
        rows$media_id <- media
      } else {
        rows <- occ[x,]
      }
      rows
    }),
    fill = TRUE
  ))

  occ_long[, image_select()] <- NULL
  
  ids <- occ_long$media_id[!is.na(occ_long$media_id)]
  
  metadata <- data.frame(filter_metadata(
    media_metadata(ids = ids),
    media_filter
  ))
  
  if (nrow(metadata) == 0) {
    stop("Metadata could not be found for any images for the species provided.")
  } 
  
  # Select only the columns we want
  names(metadata) <- rename_columns(names(metadata), type = "media")
  metadata <- metadata[names(metadata) %in% wanted_columns("media")]

  # join image metadata with occurrence data
  all_data <- merge(metadata, occ_long, by = "media_id")
  
  # download images
  urls <- media_urls(all_data$media_id)
  outfiles <- media_outfiles(all_data$media_id, all_data$mimetype, download_dir)

  if (verbose) {
    message("Downloading ", length(urls), " media files...")
  }
  download_media(urls, outfiles, verbose)
  all_data$download_path <- outfiles
  if (verbose) {
    message("\n",nrow(all_data), " files were downloaded to ", download_dir)
  }
  attr(all_data, "data_type") <- "media"
  query <- data_request(taxa, filter, geolocate, select)
  attr(all_data, "data_request") <- query
  
  if (caching) {
    write_cache_file(object = all_data, data_type = "media",
                     cache_file = cache_file)
  }
  return(all_data)
}

# Construct url paths to where media will be downloaded from
# Returns a vector of urls; one per id
media_urls <- function(ids) {
  url <- parse_url(server_config("images_base_url"))
  unlist(lapply(seq_len(length(ids)), function(x) {
    url$path <- c("image", as.character(ids[x]), "original")
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

# Download images in batches of 124. The limit is due to a max on the
# number of concurrently open connections.
# The asynchronous method is slightly quicker than downloading all
# images in a loop
download_media <- function(urls, outfiles, verbose) {
  if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }
  calls <- ceiling(length(urls) / 124)
  results <- lapply(seq_len(calls - 1), function(x) {
    start <- 1 + (x - 1) * 124
    end <- x * 124
    cc <- Async$new(
      urls = urls[start:end]
    )
    res <- cc$get(disk = outfiles[start:end])
    if (verbose) {
      val <- (end / length(urls))
      setTxtProgressBar(pb, val)
    }
  })
  
  # TODO: Extract this part into to a function like ala_async_get()
  # Download remaining images
  start <- 1 + (calls - 1) * 124
  end <- length(urls)
  cc <- Async$new(
    urls = c(urls[start:end]), 
    headers = list(
      "User-Agent" = galah_version_string()
    )
  )
  res <- cc$get(disk = outfiles[start:end])
  if (verbose) {
    val <- (end / length(urls))
    setTxtProgressBar(pb, val)
  }
}

# Get metadata for a list of media ids
media_metadata <- function(ids) {
  res <- ala_POST(
    url = server_config("images_base_url"),
    path = "/ws/imageInfoForList",
    body = list(imageIds = ids),
    encode = "json"
    )

  # parse result and convert to data.frame
  data <- fromJSON(res)
  # suppress warnings caused by different list lengths
  df <- suppressWarnings(data.table::rbindlist(data$results))
  return(df)
}

# Use media filter to filter returned results
# These are filter on metadata values, as opposed to filter on occurrence
# records
filter_metadata <- function(metadata, filter) {
  if (is.null(filter)) {
    return(metadata)
  }
  for (i in seq_len(nrow(filter))) {
    val <- filter[i,]$value[[1]]
    filter_name <- filter[i,]$name
    metadata <- metadata[metadata[[filter_name]] == val]
  }
  return(metadata)
}
