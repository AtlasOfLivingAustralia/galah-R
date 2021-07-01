#' Data quality profiles
#'
#' The ALA provides a number of pre-built data quality profiles for 
#' filtering data according to quality checks. A data quality profile can
#' be specified in the \code{profile} argument in \code{\link{select_filters}()}
#' and used to filter searches in \code{\link{ala_occurrences}()},
#' \code{\link{ala_counts}()} and \code{\link{ala_species}()}.
#'
#' @export find_profiles
#' @return A \code{data.frame} of available profiles
#' @seealso This function gives viable profle names for passing to
#' \code{\link{select_filters}()}. For more detail on a given profile see
#' \code{\link{find_profile_attributes}()}.
#' @examples \dontrun{
#' # Get available profiles
#' profile_df <- find_profiles()
#' # Values given in the 'shortName' column are accepted by select_filter(), i.e.
#' select_filters(profile = profile_df$shortName[1])
#' # is equivalent to:
#' select_filters(profile = "ALA")
#' }

# this will return names and descriptions of data profiles
# should id be exposed to the user?
find_profiles <- function() {
  # return only enabled profiles?
  url <- server_config("data_quality_base_url")
  resp <- ala_GET(url, "api/v1/profiles", list(enabled = "true"))
  return(resp[wanted_columns(type = "profile")])
}

#' Get data filters for a specified data quality profile
#'
#' Each data quality profile is made up of a series of filters. While some users
#' may wish to simply trust the default filters, it is often useful to check
#' what information they return, particularly if advanced customization is needed.
#' This function gives all of the arguments built into a specific profile.
#'
#' @param profile \code{string}: a data quality profile name, short name or id.
#' See \code{\link{find_profiles}()} for valid filters
#' @export find_profile_attributes
#' @return A \code{data.frame} of profile attributes, consisting of a
#' free text \code{description} and the actual \code{filter} used.
#' @seealso \code{\link{find_profiles}()} for a list of valid profiles;
#' \code{\link{select_filters}()} for how to include this information in a data
#' query.
#' @examples
#' profile_info <- find_profile_attributes("CSDM")
#' profile_info$description # free-text description of each filter in the "CSDM" profile

find_profile_attributes <- function(profile) {
  # check if is numeric or can be converted to numeric
  short_name <- profile_short_name(profile)
  if (is.na(short_name)) {
    stop(profile, " is not a valid data quality id, short name or name. Use
          `find_profiles` to list valid profiles.")
  }
  url <- server_config("data_quality_base_url")
  resp <- ala_GET(url, "api/v1/quality/activeProfile",
                  list(profileName = short_name))
  filters <- data.table::rbindlist(resp$categories$qualityFilters)
  subset(filters, select = wanted_columns("quality_filter"))
}

profile_short_name <- function(profile) {
  valid_profiles <- find_profiles()
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
