#' Specify a data profile
#'
#' 'profiles' are groups of filters that are pre-applied by the ALA, allowing
#' rapid narrowing of results to the most relevant data. Note that only one
#' profile can be loaded at a time; if multiple profiles are given, the first
#' valid profile is used.
#'
#' @param ... a profile name. Valid values can be seen using `show_all_profiles()$shortName`
#' @export

galah_data_profile <- function(...){
  
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
  
  # check to see if any of the inputs are a data request
  checked_dots <- detect_data_request(dots)
  if(!inherits(checked_dots, "quosures")){
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  }else{
    is_data_request <- FALSE
  }

  # this code is basically taken from galah_identify()
  if (length(dots) > 0) {

    # basic checking
    check_queries(dots) # capture named inputs
    input_profile <- parse_basic_quosures(dots) # convert dots to query
    
    # check which inputs are valid
    # note that in galah_filter, this is dependent on getOption("galah_config")$run_checks
    # not required here as show_all_profiles is pretty fast
    valid_profile <- check_profile(input_profile)
    
    result <- tibble(data_profile = valid_profile)
    
  
  }else{
    result <- tibble()
  }
  
  # if a data request was supplied, return one
  attr(result, "call") <- "galah_data_profile"
  if (is_data_request) {
    update_galah_call(data_request, data_profile = result)
  } else {
    result
  }
}



check_profile <- function(query, error_call = caller_env()){
  valid_check <- query %in% show_all_profiles()$shortName
  if(!any(valid_check)){    
    bullets <- c(
      "The value passed to `galah_profile` isn't a valid profile name",
      i = "Use `show_all_profiles` to lookup profile information."
    )
    abort(bullets, call = error_call)
  }else{
    return(query[which(valid_check)[1]])
  }
}