#' New, internal function for getting metadata
#' @param An object of class `data_query` containing fields 'ids' and 'type'
#' @keywords Internal
#' @noRd
collect_media_metadata <- function(.data){
  
  verbose <- pour("package", "verbose")
  
  if(verbose){
    inform(glue("Downloading metadata for {length(.data$ids)} files"))
  }
  metadata <- media_metadata(paste0(.data$url, .data$ids))
  
  if(is.null(metadata)){
    inform("Calling the metadata API failed for `atlas_media`")
    return(tibble())
  }
  
  # i.e. service is online, but no data available
  if(nrow(metadata) < 1) {
    if(verbose){
      system_down_message("collect() with type = 'media-metadata'")
    }
    return(df_long)
  } 
  
  # Select only the columns we want
  colnames(metadata) <- rename_columns(names(metadata), type = "media")
  file_ext <- str_extract(metadata$original_file_name, ".[:alpha:]+$")
  metadata <- metadata[colnames(metadata) %in% wanted_columns("media")]
  metadata$file_extension <- file_ext

  tibble(metadata)
}


#' Internal function to get metadata for a list of media ids
#' @noRd
#' @keywords Internal
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
media_metadata <- function(urls){
  verbose <- pour("package", "verbose")
  if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }

  n <- seq_along(urls)
  result <- lapply(n, function(a){
    x <- url_GET(urls[a])
    if (verbose) {
      val <- (a / max(n))
      setTxtProgressBar(pb, val)
    }
    as.data.frame(x[lengths(x) == 1])
  })
  bind_rows(result) |> tibble()
}