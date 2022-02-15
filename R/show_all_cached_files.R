#' @rdname show_all_minifunctions
#' @export

show_all_cached_files <- function() {
  # return a data.frame of all cached files
  metadata_path <- file.path(getOption("galah_config")$cache_directory,
                             "metadata.rds")
  if (!file.exists(metadata_path)) {
    directory <- getOption("galah_config")$cache_directory
    inform(glue("No cached file information was found in {directory}."))
    return()
  }
  readRDS(metadata_path)
}
