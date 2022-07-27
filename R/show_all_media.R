#' @rdname show_all_minifunctions
#' @param ids either a `tibble` from `atlas_occurrences` or a vector giving media ids (parsed or unparsed)
#' @export

show_all_media <- function(df){
  
  # run checks that 1. a df is supplied, 2. it contains requisite columns
  assert_that(inherits(df, "data.frame"))
  default_cols <- c("images", "videos", "sounds")
  if(!any(colnames(ids) %in% default_cols)){
    return(tibble())
  }
  
  # convert multi-column IDs to single-column
  cols <- df[, colnames(df) %in% default_cols]
  if(ncol(cols) > 1){
    ids <- apply(cols, 1, function(a){paste(a[a != ""], collapse = " | ")})
  }

  # split strings
  ids_list <- str_split(ids, pattern = "\\s\\|\\s")
  ids_vector <- unlist(ids_list)
  
  # create long-form version of non-media rows
  df_long <- df[
    rep(seq_len(nrow(df)), lengths(ids_list)),
    !(colnames(df) %in% default_cols)]
  
  # get metadata from the relevant atlas
  metadata <- media_metadata(ids = ids_vector)
  if(is.null(metadata)){
    inform("Calling the metadata API failed for `atlas_media`")
    return(tibble())
  }
  
  # DEPRECATED - capture filters from galah_filter and apply post-hoc
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
    return(df_long)
  } 

  # Select only the columns we want
  colnames(metadata) <- rename_columns(names(metadata), type = "media")
  metadata <- metadata[colnames(metadata) %in% wanted_columns("media")]

  # join image metadata with occurrence data
  tibble(cbind(df_long, metadata))
 
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