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
#' @name galah_apply_profile
#' @examples \dontrun{
#' # Apply a data quality profile to a query
#' galah_call() |> 
#'   galah_identify("reptilia") |>
#'   galah_filter(year == 2021) |>
#'   galah_apply_profile(ALA) |>
#'   atlas_counts()
#' }
#' @importFrom tibble tibble
#' @export
galah_apply_profile <- function(...){
  dots <- enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           df <- parse_quosures_basic(dots[-1]) |>
             parse_profile()
           update_data_request(dots[[1]], data_profile = df)
         },
         {
           parse_quosures_basic(dots) |>
             parse_profile()
         })
}

#' @rdname galah_apply_profile
#' @param .data An object of class `data_request`
#' @export
apply_profile <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  df <- parse_quosures_basic(dots) |>
    pluck(!!!list(1)) |>
    parse_profile()
  update_data_request(.data, data_profile = df)
}

#' Internal parsing of `profile` args
#' @importFrom glue glue
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
parse_profile <- function(dot_names, error_call = caller_env()) {
  n_args <- length(dot_names)
  if (n_args > 0) {
    if (n_args > 1) {
      bullets <- c(
        "Too many data profiles supplied.",
        x = glue("`galah_apply_profile()` accepts one profile argument, not {n_args}.")
      )
      abort(bullets, call = error_call)
    }else{
      df <- tibble(data_profile = as.character(dot_names))
    }
  }else{
    df <- NULL
  }
  return(df)
}
