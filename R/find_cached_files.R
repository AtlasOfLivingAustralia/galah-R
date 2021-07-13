#' List previously cached files
#' 
#' Uses query metadata stored in metadata.rds in the cache directory
#' @return a \code{list} of available cached files, the function used to
#' create them, and the filter object
#' @export

find_cached_files <- function() {
  # return a data.frame of all cached files
  readRDS(file.path(getOption("galah_config")$cache_directory, "metadata.rds"))
}