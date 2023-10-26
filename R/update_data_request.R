#' Internal function to update a `data_request`
#' @noRd
#' @keywords Internal
update_data_request <- function(data_request, ...){
  dots <- list(...)
  if(length(dots)[[1]] == 1){
    if(inherits(dots[[1]], "list") & is.null(names(dots[[1]]))){
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
            result <- switch(a,
                             "identify" = {
                               bind_unique_rows(data_request[[a]], dots[[a]], "search_term")
                             },
                             "filter" = {
                               bind_unique_rows(data_request[[a]], dots[[a]], "query")
                             },
                             "select" = {
                               bind_unique_rows(data_request[[a]], dots[[a]], "name")
                             }, 
                             # for below, we assume that in all other circumstances we 
                             # simply pass the most recent result (i.e. overwrite)
                             dots[[a]] # default
            )
            result
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