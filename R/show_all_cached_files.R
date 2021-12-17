#' List previously cached files
#' 
#' Uses query metadata stored in metadata.rds in the cache directory
#' @return a `list` of available cached files, the function used to
#' create them, and the filter object
#' @examples
#' ## Configure caching and create a query to cache
#' galah_config(caching = TRUE)
#' dat <- atlas_counts(group_by = galah_group_by(year))
#'
#' ## list cached files
#' show_all_cached_files()
#' 
#' @export

show_all_cached_files <- function() {
  # return a data.frame of all cached files
  metadata_path <- file.path(getOption("galah_config")$cache_directory,
                             "metadata.rds")
  if (!file.exists(metadata_path)) {
    message("No cached file information was found in ",
            getOption("galah_config")$cache_directory)
    return()
  }
  readRDS(metadata_path)
}
