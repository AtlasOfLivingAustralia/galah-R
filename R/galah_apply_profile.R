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
#' @importFrom tibble tibble
#' @export

galah_apply_profile <- function(...){
  # browser()
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  df <- parse_profile(parsed_dots$data)
  if(is.null(parsed_dots$data_request)){
    df
  }else{
    update_data_request(parsed_dots$data_request, data_profile = df)
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

#' Internal parsing of `profile` args
#' @noRd
#' @keywords Internal
parse_profile <- function(dot_names, error_call = caller_env()) {
  if (length(dot_names) > 0) {
    
    if (length(dot_names) > 1) {
      bullets <- c(
        "Too many data profiles supplied.",
        i = "`galah_apply_profile()` accepts only one profile at a time."
      )
      abort(bullets, call = error_call)
    } else{
      
        valid_check <- dot_names %in% show_all_profiles()$shortName
        if (!any(valid_check)) {
          bullets <- c(
            "Invalid profile name.",
            i = "Use `show_all(profiles)` to see all valid profiles."
          )
          abort(bullets, call = error_call)
        } else {
          profile <- dot_names[which(valid_check)[1]]
        }
    }

    df <- tibble(data_profile = as.character(profile))
  }
  return(df)
}
