#' Search for which quality filters are applied by a data quality profile
#'
#' Each data quality profile is made up of a series of filters. While some users
#' may wish to simply trust the default filters, it is often useful to check
#' what information they return, particularly if advanced customization is needed.
#' This function gives all of the arguments built into a specific profile.
#'
#' @param profile `string`: a data quality profile name, short name or id.
#' See [show_all_profiles()] for valid filters
#' @return A `data.frame` of profile attributes, consisting of a
#' free text `description` and the actual `filter` used.
#' @seealso [show_all_profiles()] for a list of valid profiles;
#' [galah_filter()] for how to include this information in a data
#' query.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' To find all the data quality arguments used in the profile "CSDM"
#' ```{r, comment = "#>", collapse = TRUE}
#' search_profile_attributes("CSDM")
#' ```
#' 
#' Then get a free-text description of each filter used in the "CSDM" profile
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' profile_info <- search_profile_attributes("CSDM")
#' profile_info$description
#' ```
#' 
#' @export search_profile_attributes

search_profile_attributes <- function(profile) {
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