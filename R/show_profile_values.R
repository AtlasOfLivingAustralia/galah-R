#' @param profile `string`: a data quality profile name, short name or id.
#' See [show_all_profiles()] for valid filters
#' @rdname show_values
#' @export show_profile_values

show_profile_values <- function(profile) {
  
  if (getOption("galah_config")$atlas != "Australia") {
    bullets <- c(
      "Data profiles are only available for the Australian atlas"
    )
    abort(bullets, call = caller_env())
  }
  
  # check if is numeric or can be converted to numeric
  short_name <- profile_short_name(profile)
  if (is.na(short_name)) {
    bullets <- c(
      "Invalid data quality ID.",
      i = "Use `show_all_profiles` to see a listing of valid profiles.",
      x = glue("{profile} is not a valid ID, short name or name.")
    )
    abort(bullets, call = caller_env())
  }
  url <- server_config("data_quality_base_url")
  resp <- atlas_GET(url, "api/v1/quality/activeProfile",
                  list(profileName = short_name))
  if(is.null(resp)){
    bullets <- c(
      "Calling the API failed for `search_profile_attributes`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct."
    )
    inform(bullets)
    tibble()
  }else{
    filters <- rbindlist(resp$categories$qualityFilters)
    subset(filters, select = wanted_columns("quality_filter")) |> as_tibble()
  }  
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