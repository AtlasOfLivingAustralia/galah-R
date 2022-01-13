#' List previously cached files
#' 
#' When using caching by setting `galah_config(caching = TRUE)`, show a list of 
#' all previously cached files. This function acheives this by using query 
#' metadata stored in metadata.rds in the cache directory
#' 
#' @return A `list` of 
#' available cached files, the function used to create them, and the filter 
#' object
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Configure caching and create a query to cache with [galah_config()]
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(caching = TRUE)
#' dat <- atlas_counts(group_by = galah_group_by(year))
#' ```
#'
#' Show a listing of previously cached files
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' show_all_cached_files()
#' ```
#' 
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
