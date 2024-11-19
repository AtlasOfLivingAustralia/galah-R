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
#' @importFrom dplyr bind_cols
#' @name arrange.data_request
#' @export
arrange.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  if(length(parsed_dots) == 2 & 
     all(names(parsed_dots) %in% c("variable", "direction"))){
    .data$arrange <- as.list(parsed_dots) |> 
      as.data.frame() |>
      tibble()
  }else{
    .data$arrange <- tibble(variable = parsed_dots, 
                            direction = "ascending")    
  }
  return(.data)
}

#' @rdname arrange.data_request
#' @export
arrange.metadata_request <- arrange.data_request
