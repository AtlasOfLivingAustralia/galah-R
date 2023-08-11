#' Internal version of `collect()` for `type = "media"`
#' @param object of class `data_response`, from `compute()`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collect_media_metadata <- function(.data){
  result <- query_API(.data)

  ## Error catching  
  # if(is.null(metadata)){
  #   inform("Calling the metadata API failed for `atlas_media`")
  #   return(tibble())
  # }
  # 
  # # i.e. service is online, but no data available
  # if (nrow(metadata) == 0) {
  #   if(pour("package", "verbose")){
  #     system_down_message("search_media")
  #   }
  #   return(df_long)
  # } 
  
  # # Select only the columns we want
  # colnames(metadata) <- rename_columns(names(metadata), type = "media")
  # file_ext <- str_extract(metadata$original_file_name, ".[:alpha:]+$")
  # metadata <- metadata[colnames(metadata) %in% wanted_columns("media")]
  # metadata$file_extension <- file_ext
  # metadata$row <- as.integer(names(ids_vector))

  return(result)
}