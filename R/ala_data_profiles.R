#' Data quality profiles
#'
#' List available profiles for data filtering 
#'
#' @export ala_data_profiles

# this will return names and descriptions of data profiles
# should id be exposed to the user?
ala_data_profiles <- function() {
  # return only enabled profiles?
  url <- getOption("ALA4R_server_config")$base_url_data_quality
  resp <- ala_GET(url, "api/v1/profiles", list(enabled = "true"))
  return(resp[wanted_columns(type = "profile")])
}

#' Get data filters for a  data quality profile
#'
#' @param profile string: a data quality profile name, short name or id.
#' See `ala_data_profiles` for valid filters
#' @export ala_quality_filters

ala_quality_filters <- function(profile) {
  valid_profiles <- ala_data_profiles()
  # check if is numeric or can be converted to numeric
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
  if (is.na(short_name)) {
    stop(profile, " is not a valid data quality id, short name or name. Use
          `ala_data_profiles` to list valid profiles.")
  }

  url <- getOption("ALA4R_server_config")$base_url_data_quality
  resp <- ala_GET(url, "api/v1/quality/activeProfile",
                  list(profileName = short_name))
  filters <- data.table::rbindlist(resp$categories$qualityFilters)
  subset(filters, select = wanted_columns("quality_filter"))
}
