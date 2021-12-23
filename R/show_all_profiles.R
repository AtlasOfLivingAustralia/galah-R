#' Data quality profiles
#'
#' The ALA provides a number of pre-built data quality profiles for 
#' filtering data according to quality checks. A data quality profile can
#' be specified in the `profile` argument in [galah_filter()]
#' and used to filter searches in [atlas_occurrences()],
#' [atlas_counts()] and [atlas_species()].
#'
#' @export show_all_profiles
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) of 
#' available profiles
#' @seealso This function gives viable profile names for passing to
#' [galah_filter()]. For more detail on a given profile see
#' [find_profile_attributes()].
#' @examples 
#' # Get available profiles
#' profile_df <- show_all_profiles()
#' # Values given in the 'shortName' column are accepted by select_filter(), i.e.
#' galah_filter(profile == profile_df$shortName[1])
#' # is equivalent to:
#' galah_filter(profile == "ALA")
#' 

# this will return names and descriptions of data profiles
# should id be exposed to the user?
show_all_profiles <- function() {
  
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_profiles")
  url <- server_config("data_quality_base_url") #  this doesn't run a query,
    # but does ping an error if the selected atlas doesn't support profiles
 
  if(update_needed){ # i.e. we'd like to run a query
    # return only enabled profiles?
    resp <- atlas_GET(url, "api/v1/profiles", list(enabled = "true"))
    if(is.null(resp)){ # if calling the API fails, return cached data
      message("Calling the API failed for `show_all_profiles`; Returning cached values")
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
 

#' Get data filters for a specified data quality profile
#'
#' Each data quality profile is made up of a series of filters. While some users
#' may wish to simply trust the default filters, it is often useful to check
#' what information they return, particularly if advanced customization is needed.
#' This function gives all of the arguments built into a specific profile.
#'
#' @param profile `string`: a data quality profile name, short name or id.
#' See [show_all_profiles()] for valid filters
#' @export find_profile_attributes
#' @return A `data.frame` of profile attributes, consisting of a
#' free text `description` and the actual `filter` used.
#' @seealso [show_all_profiles()] for a list of valid profiles;
#' [galah_filter()] for how to include this information in a data
#' query.
#' @examples
#' profile_info <- find_profile_attributes("CSDM")
#' profile_info$description # free-text description of each filter in the "CSDM" profile

find_profile_attributes <- function(profile) {
  # check if is numeric or can be converted to numeric
  short_name <- profile_short_name(profile)
  if (is.na(short_name)) {
    stop(profile, " is not a valid data quality id, short name or name. Use
          `show_all_profiles` to list valid profiles.")
  }
  url <- server_config("data_quality_base_url")
  resp <- atlas_GET(url, "api/v1/quality/activeProfile",
                  list(profileName = short_name))
  filters <- data.table::rbindlist(resp$categories$qualityFilters)
  subset(filters, select = wanted_columns("quality_filter")) |> as_tibble()
}

profile_short_name <- function(profile) {
  valid_profiles <- show_all_profiles()
  short_name <- NA
  if (suppressWarnings(!is.na(as.numeric(profile)))) {
    # assume a profile id has been provided
    short_name <- valid_profiles[match(as.numeric(profile),
                                       valid_profiles$id),]$shortName
  } else {
    # try to match a short name or a long name
    if (profile %in% valid_profiles$name) {
      short_name <- valid_profiles[match(profile,
                                         valid_profiles$name), ]$shortName
    } else {
      if (profile %in% valid_profiles$shortName) {
        short_name <- profile
      }
    }
  }
  short_name
}
