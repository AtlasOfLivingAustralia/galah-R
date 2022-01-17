#' List data quality profiles
#'
#' The ALA provides a number of pre-built data quality profiles for 
#' filtering data according to quality checks. A data quality profile can
#' be specified in the `profile` argument in [galah_filter()]
#' and used to filter searches in [atlas_occurrences()],
#' [atlas_counts()] and [atlas_species()].
#'
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) of 
#' available profiles
#' @seealso This function gives viable profile names for passing to
#' [galah_filter()]. For more detail on a given profile see
#' [search_profile_attributes()].
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
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
#' 
#' @export show_all_profiles
show_all_profiles <- function() {
  
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_profiles")
  url <- server_config("data_quality_base_url") #  this doesn't run a query,
    # but does ping an error if the selected atlas doesn't support profiles
 
  if(update_needed){ # i.e. we'd like to run a query
    # return only enabled profiles?
    resp <- atlas_GET(url, "api/v1/profiles", list(enabled = "true"))
    if(is.null(resp)){ # if calling the API fails, return cached data
      df <- galah_internal_cache()$show_all_profiles
      attr(df, "ARCHIVED") <- NULL # remove identifying attributes
    }else{
      df <- as_tibble(resp[wanted_columns(type = "profile")])
      galah_internal_cache(show_all_profiles = df)
    }    
  }else{
     df <- galah_internal_cache()$show_all_profiles
  }
  df
}