#' This should parse out a request object and return quosures thereafter
#' @noRd
#' @keywords Internal
detect_request_object <- function(dots){
  if (length(dots) > 0) {
    call_string <- rlang::get_expr(dots)[[1]] |> 
      rlang::quo_get_expr() |>
      deparse() |>
      glue::glue_collapse(sep = " ") # captures multi-lines
    # note: no leading "^" below, 
    # because pipes can parse to e.g. `galah_identify(galah_call(...`
    types <- c(
      "galah_call\\(",
      "request_data\\(",
      "request_metadata\\(",
      "request_files\\(",
      "^~.$",
      "^.$") |>
      paste(collapse = "|")
    if (stringr::str_detect(call_string, types)) { # note: "~." or "." indicate presence of the magrittr pipe (%>%)
      eval_request <- rlang::eval_tidy(dots[[1]])
      c(list(eval_request), dots[-1])
    }else{
      dots
    }
  }else{
    NULL
  }
}

#' Internal function to update a `data_request`
#' @noRd
#' @keywords Internal
update_request_object <- function(x, ...){
  class_tr <- class(x)
  dots <- list(...)
  if(length(dots)[[1]] == 1){
    if(inherits(dots[[1]], "list") & is.null(names(dots))){
      dots <- dots[[1]]
    }
  }
  result <- purrr::map(
    names(x), # i.e. for all slots in object of class `data_request` or `metadata_request`
    function(a){
      if(any(names(dots) == a)){ # object is present in `x`
        if(is.null(x[[a]])){ # slot in `x` is empty
          dots[[a]]
        }else{ # slot is filled
          if(is.null(dots[[a]])){ # if nothing has been supplied, retain source
            x[[a]]
          }else{ # both supplied and source contain data
            switch(a,
                   "identify" = {
                     bind_unique_rows(x[[a]], dots[[a]], "search_term")
                   },
                   "filter" = {
                     bind_unique_rows(x[[a]], dots[[a]], "query")
                   },
                   "select" = {
                     update_select(x[[a]], dots[[a]])
                   }, 
                   # for below, we assume that in all other circumstances we 
                   # simply pass the most recent result (i.e. overwrite)
                   dots[[a]] # default
            )
          }      
        }
      }else{ # if supplied object is not named in `data_request`
        x[[a]]
      }
    })
  names(result) <- names(x)
  
  # check if any names in `dots` have been missed from `results`
  missing_names <- !(names(dots) %in% names(result))
  if(any(missing_names)){
    result <- append(result, dots[missing_names])
  }
  structure(result,
            class = class_tr)
}

#' Internal function to join together two `select` objects
#' @noRd
#' @keywords Internal
update_select <- function(x, y){
  quosure_check_x <- purrr::map(x, rlang::is_quosure) |> unlist()
  quosure_check_y <- purrr::map(y, rlang::is_quosure) |> unlist()
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
#' @noRd
#' @keywords Internal
bind_unique_rows <- function(x, y, column){
  result <- list(x, y) |> 
    dplyr::bind_rows() |>
    tibble::tibble() 
  dplyr::filter(result, !duplicated(result[[column]]))
}