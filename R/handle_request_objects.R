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

#' Internal function to update a `data_request` or `metadata_request`
#' @param x an object to update
#' @param ... named entry to add
#' @noRd
#' @keywords Internal
update_request_object <- function(x,
                                  ...,
                                  error_call = rlang::caller_env()){
  # collect supplied information
  x_class <- class(x)
  x_names <- names(x)
  dots <- list(...)
  dot_names <- names(dots)

  # ensure only one value is supplied
  if(length(dots) > 1){
    cli::cli_warn("Can only update a request with one object at a time; skipping")
    x
  }

  # ensure it is named
  if(length(dot_names) < 1){
    cli::cli_warn("Error updating `data_request` object - all entries must be named - skipping",
                  call = error_call)
    x
  }

  # if this slot is already populated, update or overwrite
  if(any(x_names == dot_names)){
    x[[dot_names]] <- switch(dot_names,
                           "identify" = {
                              bind_unique_rows(x[[dot_names]],
                                               dots[[dot_names]],
                                               "search_term")
                            },
                            "filter" = {
                              bind_unique_rows(x[[dot_names]],
                                               dots[[dot_names]],
                                               "query")
                            },
                            "select" = {
                              update_select(x[[dot_names]],
                                            dots[[dot_names]])
                            }, 
                            # for below, we assume that in all other circumstances we 
                            # simply pass the most recent result (i.e. overwrite)
                            dots[[dot_names]] # default
                            )
    structure(x, class = x_class)
  }else{ # if not already present, add to end of object
    structure(c(x, dots), class = x_class)
  }
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