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