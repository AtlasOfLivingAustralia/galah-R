#' @rdname show_all_minifunctions
#' @param ids either a `tibble` from `atlas_occurrences` or a vector giving media ids (parsed or unparsed)
#' @export

show_all_media <- function(ids){
  
  assert_that(inherits(ids, "data.frame") | inherits(ids, "character"))
  default_cols <- c("images", "videos", "sounds")
  
  if(inherits(ids, "data.frame")){
    if(any(colnames(ids) %in% default_cols)){
      cols <- ids[, default_cols[default_cols %in% colnames(ids)]]
    }else{
      cols <- ids
    }
    ids_vector <- do.call(c, cols)
  }else{
    ids_vector <- ids
  }
  ids_vector <- ids_vector[!is.na(ids_vector) & ids_vector != ""]
  names(ids_vector) <- NULL

  # split strings
  ids_vector <- unlist(str_split(ids_vector, pattern = "\\s\\|\\s"))
   
  # # occurrence data.frame has one row per occurrence record and stores all media
  # # ids in a single column; this code splits the media ids and creates one row
  # # per media id in the returned data.frame
  # occ_long <- data.frame(rbindlist(
  #   lapply(seq_len(nrow(occ)), function(x) {
  #     # get all the image, video and sound columns into one row
  #     splt_media <- unlist(str_split(occ[x,][image_fields()],
  #                                    pattern = "\\s\\|\\s"))
  #     media <- splt_media[nchar(splt_media) > 1 & splt_media != "NA"]
  # 
  #     if (length(media) > 0) {
  #       rows <- occ[x,][rep(seq_len(nrow(occ[x,])), each = length(media)), ]
  #       rows$media_id <- media
  #     } else {
  #       rows <- occ[x,]
  #     }
  #     rows
  #   }),
  #   fill = TRUE
  # ))
  # occ_long[, image_fields()] <- NULL
  # ids <- occ_long$media_id[!is.na(occ_long$media_id)]

  # get metadata from the relevant atlas
  metadata <- media_metadata(ids = ids_vector)
  if(is.null(metadata)){
    inform("Calling the metadata API failed for `atlas_media`")
    return(tibble())
  }
  # metadata <- filter_metadata(online_metadata, media_filter)

  # i.e. service is online, but no data available
  if (nrow(metadata) == 0) {
    if(verbose){
      bullets <- c(
        "Calling the API failed for `atlas_media`.",
        i = "This might mean that the ALA system is down. Double check that your query is correct.",
        i = "If you continue to see this message, please email support@ala.org.au."
      )
      inform(bullets)
    }
    return(tibble())
  } 

  # Select only the columns we want
  colnames(metadata) <- rename_columns(names(metadata), type = "media")
  metadata[colnames(metadata) %in% wanted_columns("media")] |> tibble()

  # join image metadata with occurrence data
  # all_data <- merge(metadata, occ_long, by = "media_id") |> as_tibble()
  # all_data
  
}

# Get metadata for a list of media ids
media_metadata <- function(ids){
  
  result <- lapply(ids, function(a){
    x <- atlas_GET(
      url = server_config("images_base_url"),
      path = paste0("image/", a)
    )
    as.data.frame(x[lengths(x) == 1])
  })
  df <- rbindlist(result, fill = TRUE)
  as.data.frame(df)
}

# # Get metadata for a list of media ids
## This is original code, but currently fails
# media_metadata <- function(ids) {
#   res <- atlas_POST(
#     url = server_config("images_base_url"),
#     path = "/ws/imageInfoForList",
#     body = list(imageIds = ids),
#     encode = "json"
#     )
#   if(is.null(res)){
#     return(NULL)
#   }else{
#     # parse result and convert to data.frame
#     data <- fromJSON(res)
#     # suppress warnings caused by different list lengths
#     df <- suppressWarnings(rbindlist(data$results))
#     return(df)
#   }
# }

# # Use media filter to filter returned results
# # These are filter on metadata values, as opposed to filter on occurrence
# # records
# filter_metadata <- function(metadata, filter) {
#   if (is.null(filter)) {
#     return(metadata)
#   }
#   for (i in seq_len(nrow(filter))) {
#     val <- filter[i,]$value[[1]]
#     filter_name <- filter[i,]$name
#     metadata <- metadata[metadata[[filter_name]] == val]
#   }
#   return(metadata)
# }
## no longer in use