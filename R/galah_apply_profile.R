#' Apply a data quality profile
#'
#' @description
#' A 'profile' is a group of filters that are pre-applied by the ALA. Using a 
#' data profile allows a query to be filtered quickly to the most relevant or 
#' quality-assured data that is fit-for-purpose. For example, the "ALA" profile
#' is designed to exclude lower quality records, whereas other profiles apply 
#' filters specific to species distribution modelling (e.g. CDSM).
#' 
#' Note that only one profile can be loaded at a time; if multiple profiles are 
#' given, the first valid profile is used.
#' 
#' For more bespoke editing of filters within a profile, use 
#' [filter.data_request()].
#' @param .data An object of class `data_request`
#' @param ... a profile name. Should be a `string` - the name or abbreviation 
#'    of a data quality profile to apply to the query. Valid values can be seen 
#'    using `show_all(profiles)`
#' @return An updated `data_request` with a completed `data_profile` slot.
#' @seealso [show_all()] and [search_all()] to look up available data profiles. 
#' [filter.data_request()] can be used for more bespoke editing of individual data 
#' profile filters.
#' @name apply_profile
#' @examples \dontrun{
#' # Apply a data quality profile to a query
#' galah_call() |> 
#'   identify("reptilia") |>
#'   filter(year == 2021) |>
#'   apply_profile(ALA) |>
#'   atlas_counts()
#' }
#' @importFrom rlang enquos
#' @export
apply_profile <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  result <- parse_quosures_basic(dots) |>
    pluck(!!!list(1)) |>
    parse_profile()
  update_data_request(.data, data_profile = result)
}

#' @rdname apply_profile
#' @importFrom rlang enquos
#' @export
galah_apply_profile <- function(...){
  dots <- enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           result <- parse_quosures_basic(dots[-1]) |>
             parse_profile()
           update_data_request(dots[[1]], data_profile = result)
         },
         {
           parse_quosures_basic(dots) |>
             parse_profile()
         })
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
      as.character(dot_names)
    }
  }else{
    NULL
  }
}
