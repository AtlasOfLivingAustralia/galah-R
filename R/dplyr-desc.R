#' Descending order
#' 
#' @param ... column to order by
#' @returns A `tibble` used by `arrange.data_request()` to arrange rows of a 
#' query. 
#' @seealso \code{\link[=arrange.data_request]{arrange()}}, [galah_call()]
#' @examples \dontrun{
#' # Arrange grouped record counts by descending year
#' galah_call() |>
#'   identify("perameles") |>
#'   filter(year > 2019) |>
#'   count() |>
#'   arrange(galah::desc(year)) |>
#'   collect()
#' }
#' @export
desc <- function(...){
  dots <- rlang::enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  tibble::tibble(variable = parsed_dots,
                 direction = "descending")
}

