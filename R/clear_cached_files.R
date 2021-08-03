#' Clear or refresh previously cached files
#' 
#' Deletes cached files within the cached file directory
#' @return a \code{list} of available cached files, the function used to
#' create them, and the filter object
#' @export

clear_cached_files <- function() {
  # delete cached files and return list of files deleted in the console
  metadata_path <- file.path(getOption("galah_config")$cache_directory,
                             "metadata.rds")
  
  if (!file.exists(metadata_path)) {
    message("No cached file information was found in ",
            getOption("galah_config")$cache_directory)
    return()
  }
  metadata_file_names <- paste0(getOption("galah_config")$cache_directory, "\\", names(readRDS(metadata_path)), ".rds")
  invisible(sapply(metadata_file_names, unlink))
  invisible(unlink(metadata_path))
  message("Cache files deleted: \n", paste(metadata_file_names, collapse = "\n"))
}