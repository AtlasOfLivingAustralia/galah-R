#' Arrange rows of a query
#' 
#' @description
#' `r lifecycle::badge("experimental")`  
#' 
#' `arrange.data_request()` arranges rows of a query on the server side, meaning 
#' that prior to sending a query, the query is constructed in such a way that 
#' information will be arranged when the query is processed. Any data that is 
#' then returned by the query will have rows already pre-arranged.
#' 
#' The benefit of using `arrange()` within a `galah_call()` is that it is faster 
#' to process arranging rows on the server side than arranging rows locally on 
#' downloaded data, 
#' especially if the dataset is large or complex.
#' 
#' `arrange()` can be used within a `galah_call()` pipe, but only  
#' for queries of  `type = "occurrences-count"`. The `galah_call()` pipe must 
#' include `count()` and finish with `collect()` (see examples). 
#' 
#' @param .data An object of class `data_request`
#' @param ... Either `count` or `index`
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
#' @rdname arrange
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

#' @rdname arrange
#' @export
arrange.metadata_request <- arrange.data_request
