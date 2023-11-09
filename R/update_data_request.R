#' Internal function to update a `data_request`
#' @noRd
#' @keywords Internal
update_data_request <- function(data_request, ...){
  dots <- list(...)
  if(length(dots)[[1]] == 1){
    if(inherits(dots[[1]], "list") & is.null(names(dots))){
      dots <- dots[[1]]
    }
  }
  result <- lapply(
    names(data_request), # i.e. for all slots in object of class `data_request`
    function(a){
      if(any(names(dots) == a)){ # object is present in `data_request`
        if(is.null(data_request[[a]])){ # slot in `data_request` is empty
          dots[[a]]
        }else{ # slot is filled
          if(is.null(dots[[a]])){ # if nothing has been supplied, retain source
            data_request[[a]]
          }else{ # both supplied and source contain data
            switch(a,
                   "identify" = {
                     bind_unique_rows(data_request[[a]], dots[[a]], "search_term")
                   },
                   "filter" = {
                     bind_unique_rows(data_request[[a]], dots[[a]], "query")
                   },
                   "select" = {
                     update_select(data_request[[a]], dots[[a]])
                   }, 
                   # for below, we assume that in all other circumstances we 
                   # simply pass the most recent result (i.e. overwrite)
                   dots[[a]] # default
            )
          }      
        }
      }else{ # if supplied object is not named in `data_request`
        data_request[[a]]
      }
    })
  names(result) <- names(data_request)
  
  # check if any names in `dots` have been missed from `results`
  missing_names <- !(names(dots) %in% names(result))
  if(any(missing_names)){
    result <- append(result, dots[missing_names])
  }
  class(result) <- "data_request"
  result
}

#' Internal function to join together two `select` objects
#' @importFrom rlang is_quosure
#' @noRd
#' @keywords Internal
update_select <- function(x, y){
  quosure_check_x <- lapply(x, is_quosure) |> unlist()
  quosure_check_y <- lapply(y, is_quosure) |> unlist()
  if(any(quosure_check_y)){
    result <- append(x[quosure_check_x], y[quosure_check_y])
  }else if(any(quosure_check_x)){
    result <- x[quosure_check_x]
  }else{
    result <- list()
  }
  group_vec <- unique(c(x$group, y$group))
  if(length(group_vec) < 1){
    group_vec <- NULL
  }
  result |>
    add_summary() |>
    add_group(group = group_vec)
}

#' Internal function to join tibbles by row
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
bind_unique_rows <- function(x, y, column){
  result <- list(x, y) |> 
    bind_rows() |>
    tibble() 
  filter(result, !duplicated(result[[column]]))
}