#' Search for associated media of occurrence records
#' 
#' @description Search for media files for a set of occurrence records 
#' downloaded using [atlas_occurrences()]. `search_media()` also accepts 
#' a set of media IDs (parsed or unparsed).
#' @param df 
#'   A `tibble` of species occurrence records or media IDs.
#' @returns a `tibble` of matching media files of occurrence records or media ids
#' @examples \dontrun{
#' # Search for media files for a set of species occurrence records
#' occs <- galah_call() |>
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2001) |>
#'   atlas_occurrences()
#'   
#' search_media(occs)
#' }
#' 
#' @rdname search_media
#' @export search_media

search_media <- function(df){
  
  # run checks that 1. a df is supplied, 2. it contains requisite columns
  assert_that(inherits(df, "data.frame"))
  default_cols <- c("images", "videos", "sounds")
  if(!any(colnames(df) %in% default_cols)){
    return(df)
  }
  
  # convert multi-column IDs to single-column
  cols <- df[, colnames(df) %in% default_cols]
  if(ncol(cols) > 1){
    ids <- apply(cols, 1, function(a){
      valid_check <- (a != "") & !is.na(a)
      if(any(valid_check)){
        paste(a[valid_check], collapse = " | ")
      }else{
        ""
      }
    })
  }else{
    ids <- cols[[1]]
  }

  # split strings
  ids_list <- str_split(ids, pattern = "\\s\\|\\s")
  
  # create long-form version of non-media rows
  df_long <- df[
    rep(seq_len(nrow(df)), lengths(ids_list)),
    !(colnames(df) %in% default_cols)]
  row_index <- seq_len(nrow(df_long))
  df_long$row <- row_index
  
  # get metadata from the relevant atlas
  ids_vector <- unlist(ids_list)
  names(ids_vector) <- row_index
  ids_vector <- ids_vector[ids_vector != ""]
  metadata <- media_metadata(ids = ids_vector)
  if(is.null(metadata)){
    inform("Calling the metadata API failed for `atlas_media`")
    return(tibble())
  }

  # i.e. service is online, but no data available
  verbose <- getOption("galah_config")$package$verbose
  if (nrow(metadata) == 0) {
    if(verbose){
      system_down_message("search_media")
    }
    return(df_long)
  } 

  # Select only the columns we want
  colnames(metadata) <- rename_columns(names(metadata), type = "media")
  metadata <- metadata[colnames(metadata) %in% wanted_columns("media")]
  metadata$row <- as.integer(names(ids_vector))

  # join image metadata with occurrence data
  all_data <- merge(df_long, metadata, by = "row", all = TRUE)
  tibble(all_data[, -1])
 
}

# Get metadata for a list of media ids
media_metadata <- function(ids){
  
  result <- lapply(ids, function(a){
    url <- url_lookup("image_metadata", id = a) 
    x <- url_GET(url)
    as.data.frame(x[lengths(x) == 1])
  })
  bind_rows(result) |> tibble()
}

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
