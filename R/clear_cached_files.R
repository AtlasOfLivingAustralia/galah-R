#' Clear previously cached files
#' 
#' Deletes cached files within the cached file directory and their query 
#' metadata
#' @examples
#' \dontrun{
#' ## configure caching and create a query to cache
#' galah_config(caching = TRUE)
#' dat <- atlas_counts(group_by = galah_group_by(year))
#'
#' ## clear cached files directory
#' clear_cached_files()
#' }
#' 
#' @export

clear_cached_files <- function() {
  # delete cached files and return list of files deleted in the console
  metadata_path <- file.path(getOption("galah_config")$cache_directory,
                             "metadata.rds")
  
  if (!file.exists(metadata_path)) {
    directory <- getOption("galah_config")$cache_directory
    inform("No cached file information was found in {directory}.")
    return()
  }
  metadata_file_names <- paste0(getOption("galah_config")$cache_directory, "\\",
                                names(readRDS(metadata_path)), ".rds")
  invisible(sapply(metadata_file_names, unlink))
  invisible(unlink(metadata_path))
  list_files <- glue::glue_collapse(metadata_file_names, 
                                             sep = "\n")
  inform(glue("Cache filtes deleted: 
              
              {list_files}"))
}
