#' Order rows using column values
#' 
#' @description
#' `r lifecycle::badge("experimental")`  
#' 
#' `arrange.data_request()` arranges rows of a query on the server side, meaning 
#' that the query is constructed in such a way that information will be arranged 
#' when the query is processed. This only has an effect when used in combination
#' with \code{\link[=count.data_request]{count()}} and 
#' \code{\link[=group_by.data_request]{group_by()}}. The benefit of using 
#' `arrange()` within a `galah_call()` pipe is that it is sometimes beneficial 
#' to choose a non-default order for data to be delivered in, particularly if
#' \code{\link[=slice_head.data_request]{slice_head()}} is also called.
#' @param .data An object of class `data_request`
#' @param ... A variable to arrange the resulting tibble by. Should be one of 
#' the variables also listed in \code{\link[=group_by.data_request]{group_by()}}.
#' @returns An amended `data_request` with a completed `arrange` slot.
#' @examples \dontrun{
#' 
#' # Arrange grouped counts by ascending year
#' galah_call() |>
#'   identify("Crinia") |>
#'   filter(year >= 2020) |>
#'   group_by(year) |>
#'   arrange(year) |>
#'   count() |>
#'   collect()
#'   
#' # Arrange grouped counts by ascending record count
#' galah_call() |>
#'   identify("Crinia") |>
#'   filter(year >= 2020) |>
#'   group_by(year) |>
#'   arrange(count) |>
#'   count() |>
#'   collect()
#' 
#' # Arrange grouped counts by descending year
#' galah_call() |>
#'   identify("Crinia") |>
#'   filter(year >= 2020) |>
#'   group_by(year) |>
#'   arrange(desc(year)) |>
#'   count() |>
#'   collect()
#' }
#' @name arrange.data_request
#' @export
arrange.data_request <- function(.data, ...){
  dots <- rlang::enquos(..., .ignore_empty = "all")
  if(length(dots) < 1){
    return(.data)
  }else{
    parsed_dots <- purrr::map(dots, \(a){
      switch(expr_type(a),
             "symbol" = {rlang::as_label(a)},
             "call" = {purrr::map(rlang::quo_get_expr(a), as_string)},
             "literal" = {rlang::quo_get_expr(a)},
             cli::cli_abort("Quosure type not recognised.",
                            call = rlang::caller_env()))}) |>
      unlist()
    if(length(parsed_dots) > 1){
      result <- tibble::tibble(variable = parsed_dots[[2]],
                               direction = "descending")
    }else{
      result <- tibble::tibble(variable = parsed_dots, 
                               direction = "ascending")
    }
    update_request_object(.data,
                          arrange = result)
  }
}

#' @rdname arrange.data_request
#' @export
arrange.metadata_request <- arrange.data_request
