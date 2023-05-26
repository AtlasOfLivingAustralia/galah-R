#' function to check whether `type` arg is supplied to `collapse()` or `compute()`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_type <- function(type){
  if(missing(type)){
    bullets <- c("Argument `type` is missing, with no default",
                 i = "`type` must be one of 'counts', 'species', 'occurrences' or 'media'")
    abort(bullets)
  }
}

#' Internal function to confirm requisite login information has been provided
#' Called by `compute()`
#' @noRd
#' @keywords Internal
#' @importFrom rlang caller_env
check_login <- function(.data, error_call = caller_env()){
  if(is_gbif()){
    if(.data$opts$userpwd == ":"){
      abort("GBIF requires a username and password to download occurrences or species")
    }
  }else{
    if(.data$query$email == ""){
      bullets <- c(
        "No user email was found.",
        i = glue("To download occurrence records you must provide a valid email ",
                 "address registered with the selected atlas using `galah_config(email = )`")
      )
      abort(bullets, call = error_call)
    }
  }
}

#' Check whether geolocate functions have >1 argument
#' @noRd
#' @keywords Internal
check_n_inputs <- function(dots, error_call = caller_env()) {
  if(length(dots) > 1){
    n_geolocations <- length(dots)
    bullets <- c(
      "More than 1 spatial area provided.",
      "*" = glue("Using first location, ignoring additional {n_geolocations - 1} location(s).")
    )
    warn(bullets, call = caller_env())
  }
}