#' List previously cached files
#' 
#' Uses query metadata stored in metadata.rds in the cache directory
#' @return a \code{list} of available cached files, the function used to
#' create them, and the filter object
#' @examples
#' ## configure caching and create a query to cache
#' \dontrun{
#' galah_config(caching = TRUE)
#' dat <- ala_counts(group_by = "year")
#'
#' ## list cached files
#' find_cached_files()
#' }
#' @export

find_cached_files <- function() {
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