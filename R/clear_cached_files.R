#' Clear previously cached files
#' 
#' Deletes cached files within the cached file directory and their query 
#' metadata
#' 
#' @return No return value; called for side effect of removing files
#' @examples \dontrun{
#' # First set caching to true with [galah_config()]
#' galah_config(caching = TRUE)
#' 
#' # Then create a data query.
#' # The data you download will be cached in a temporary directory.
#' dat <- atlas_counts(group_by = galah_group_by(year))
#'
#' # To clear your cached files directory, use `clear_cached_files()`
#' clear_cached_files()
#' }
#' @importFrom glue glue_collapse
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang inform
#' @export

clear_cached_files <- function() {
  # delete cached files and return list of files deleted in the console
  cache_directory <- pour("package", "cache_directory")
  metadata_path <- file.path(cache_directory, "metadata.rds")
  
  if (!file.exists(metadata_path)) {
    inform("No cached file information was found in {cache_directory}.")
    return()
  }
  metadata_file_names <- paste0(cache_directory,
                                "\\",
                                names(readRDS(metadata_path)), 
                                ".rds")
  invisible(sapply(metadata_file_names, unlink))
  invisible(unlink(metadata_path))
  list_files <- glue_collapse(metadata_file_names, sep = "\n")
  inform(glue("Cache files deleted: 
              
              {list_files}"))
}
