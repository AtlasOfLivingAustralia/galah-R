#' Apply a data quality profile
#'
#' A 'profile' is a group of filters that are pre-applied by the ALA. Using a 
#' data profile allows a query to be filtered quickly to the most relevant or 
#' quality-assured data that is fit-for-purpose. For example, the "ALA" profile
#' is designed to exclude lower quality records, whereas other profiles apply 
#' filters specific to species distribution modelling (e.g. CDSM).
#' 
#' Note that only one profile can be loaded at a time; if multiple profiles are 
#' given, the first valid profile is used.
#' 
#' For more bespoke editing of filters within a profile, use [galah_filter()]
#'
#' @param ... a profile name. Should be a `string` - the name or abbreviation 
#'    of a data quality profile to apply to the query. Valid values can be seen 
#'    using `show_all(profiles)`
#' @return A tibble containing a valid data profile value.
#' @seealso [show_all()] and [search_all()] to look up available data profiles. 
#' [galah_filter()] can be used for more bespoke editing of individual data 
#' profile filters.
#' 
#' @examples
#' # Apply a data quality profile to a query
#' galah_call() |> 
#'   galah_identify("reptilia") |>
#'   galah_filter(year == 2021) |>
#'   galah_apply_profile(ALA) |>
#'   atlas_counts()
#' 
#' @export

galah_apply_profile <- function(...){
  
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
    # note that in galah_filter, this is dependent on getOption("galah_config")$package$run_checks
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
      "Invalid profile name.",
      i = "Use `show_all(profiles)` to lookup valid profiles."
    )
    abort(bullets, call = error_call)
  }else{
    return(query[which(valid_check)[1]])
  }
}