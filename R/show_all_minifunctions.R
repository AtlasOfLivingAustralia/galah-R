#' @name show_all_minifunctions
#' @aliases show_all_assertions show_all_atlases show_all_cached_files show_all_collections show_all_datasets show_all_providers show_all_fields show_all_reasons show_all_ranks show_all_profiles show_all_values
#' @title Functions for showing detailed metadata
#' @description Some text
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble)
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' See all supported atlases
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_atlases()
#' ```
#'
#' See a listing of all valid fields and layers
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_fields()
#' ```
#'
#' Show a listing of all accepted reasons for downloading occurrence data
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_reasons()
#' ```
#' 
#' Add your download reason when configuring your session with [galah_config()]
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(download_reason_id = 3)
#' ```
#'
#' Show a list of all available data quality profiles
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_profiles()
#' ```
#' 
#' Values in the `shortName` column can be used with [galah_filter()] to 
#' narrow your data query results
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_filter(profile == "ALA")
#' ```
#' Show a listing of all taxonomic ranks
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_ranks()
#' ```
#' 
#' Use ranks with [galah_down_to()] and [atlas_taxonomy()] to get taxonomic 
#' trees
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() %>%
#'   galah_identify("fungi") %>%
#'   galah_down_to(subphylum) %>%
#'   atlas_taxonomy()
#' ```
#' 
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

NULL