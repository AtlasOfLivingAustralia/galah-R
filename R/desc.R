#' Return counts in descending order
#' 
#' Version of `dplyr::desc()` for galah queries. Uses lazy evaluation.
#' @export
desc <- function(...){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)$data
  tibble(variable = parsed_dots,
         direction = "descending")
} 