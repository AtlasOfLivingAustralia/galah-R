#' Non-generic tidyverse functions
#' 
#' Several useful functions from `{tidyverse}` packages are `generic`, meaning
#' that we can define class-specific versions of those functions and implement
#' them in galah; examples include `filter()`, `select()` and `group_by()`.
#' However, there are also functions that are only defined within tidyverse 
#' packages and are not generic. In a few cases we have re-implemented these 
#' functions in `galah`. This has the consequence of supporting consistent
#' syntax with tidyverse, at the cost of potentially introducing conflicts.
#' This can be avoided by using the `::` operator where required (see examples).
#' 
#' The following functions are included
#'  - `between()` (`dplyr`): use within `filter()` to specify a range
#'  - `desc()` (`dplyr`): use within `arrange()` to specify arrangement should be descending
#'  - `unnest()` (`tidyr`): use to 'drill down' into nested information on `fields`, `lists`, `profiles`, or `taxa`  
#'
#' These (`galah`) versions all use lazy evaluation.
#' @returns description
#' @name tidyverse_functions
NULL

#' @rdname tidyverse_functions
#' @export
between <- function(){}

#' @rdname tidyverse_functions
#' @param ... column to order by
#' @export
desc <- function(...){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)$data
  tibble(variable = parsed_dots,
         direction = "descending")
} 

#' @rdname tidyverse_functions
#' @export
unnest <- function(.data, error_call = caller_env()){
  if(!inherits(.data, "metadata_request")){
    abort("`galah::unnest` can only be used with objects of class `metadata_request`")
  }
  valid_types <- c("fields", "lists", "profiles", "taxa")
  if(!(.data$type %in% valid_types)){
    bullets <- c(
      "Invalid `type` supplied to `unnest`",
      i = "valid types are `fields`, `lists`, `profiles` or `taxa`")
    abort(bullets, call = error_call)
  }
  .data$type <- paste(.data$type, "unnest", sep = "-")
  .data
}