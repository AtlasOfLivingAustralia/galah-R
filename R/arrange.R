#' arrange rows
#' 
#' New function to arrange on server side. Experimental
#' @param .data An object of class `data_request`
#' @param ... Either `count` or `index`
#' @importFrom dplyr bind_cols
#' @rdname arrange
#' @export
arrange.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)$data
  if(length(parsed_dots) > 1){
    abort("only one variable can be passed to `arrange()`")
  }
  # if(!(parsed_dots %in% c("count", "index"))){
  #   abort("`arrange()` requires one of `index` or `count`") 
  #   # NOTE: this could change to support field name(s) supplied in `facets`
  # }
  .data$arrange <- tibble(variable = parsed_dots)
  return(.data)
}

#' @rdname arrange
#' @export
arrange.metadata_request <- arrange.data_request