#' Internal function to call `compute` for `request_metadata(type = "profiles-unnest")`
#' @noRd
#' @keywords Internal
parse_profile_values <- function(.query){
  url <- .query |>
    pluck("url") |>
    url_parse()
  profile_name <- extract_profile_name(url)
  short_name <- profile_short_name(profile_name)
  if (!pour("atlas", "region") == "Spain") {
    url$path <- paste0("/dqf-service/api/v1/data-profiles/", 
                       short_name)
  }
  result <- list(type = .query$type,
                 url = url_build(url))
  class(result) <- "query"
  return(result)
}
# this doesn't print for some reason

#' Internal function to convert between long and short names
#' for data profiles. Only used by `compute_profile_values()`
#' @noRd
#' @keywords Internal
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
  if (is.na(short_name)) {
    bullets <- c(
      "Unknown profile detected.",
      i = "See a listing of valid data quality profiles with `show_all_profiles()`."
    )
    abort(bullets, call = caller_env())
  }else{
    short_name
  }
}

#' Internal function to extract profile name from url
#' for data profiles. Only used by `compute_profile_values()`
#' @noRd
#' @keywords Internal
extract_profile_name <- function(url) {
  atlas <- pour("atlas", "region")
  if (atlas == "Spain") {
    profile_name <- url |>
      pluck("query", "profileName")
  } else {
    profile_name <- url |>
      pluck("path") |>
      sub("/dqf-service/api/v1/data-profiles/", "", x = _)
  }
  return(profile_name)
}
