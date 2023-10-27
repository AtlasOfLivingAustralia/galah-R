#' arrange rows
#' 
#' New function to arrange on server side. Experimental
#' @param q_obj An object of class `data_request`
#' @param ... Either `count` or `index`
#' @importFrom dplyr bind_cols
#' @rdname arrange
#' @export
arrange.data_request <- function(q_obj, ...){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  if(length(parsed_dots) == 2 & 
     all(names(parsed_dots) %in% c("variable", "direction"))){
    q_obj$arrange <- as.list(parsed_dots) |> 
      as.data.frame() |>
      tibble()
  }else{
    q_obj$arrange <- tibble(variable = parsed_dots, 
                            direction = "ascending")    
  }
  return(q_obj)
}

#' @rdname arrange
#' @export
arrange.metadata_request <- arrange.data_request
