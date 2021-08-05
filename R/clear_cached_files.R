#' Clear previously cached files
#' 
#' Deletes cached files within the cached file directory and their query 
#' metadata
#' @examples
#' \dontrun{
#' ## configure caching and create a query to cache
#' galah_config(caching = TRUE)
#' dat <- ala_counts(group_by = "year")
#'
#' ## clear cached files directory
#' clear_cached_files()
#' }
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
  metadata_file_names <- paste0(getOption("galah_config")$cache_directory, "\\",
                                names(readRDS(metadata_path)), ".rds")
  invisible(sapply(metadata_file_names, unlink))
  invisible(unlink(metadata_path))
  message("Cache files deleted: \n", paste(metadata_file_names,
                                           collapse = "\n"))
}