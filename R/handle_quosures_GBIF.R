# Duplicate of `handle_quosures.R` with modifications to `parse_quosures_data()`
# to return predicates in place of a data.frame

# Note that advice on constructing predicates is available here:
# https://techdocs.gbif.org/en/data-use/api-downloads

## -- Top-level parsers -- ## 
# These are called 'first' by `galah_` functions

#' parse quosures for objects of class `data_request`
#' @noRd
#' @keywords internal
parse_quosures_data_gbif <- function(dots){
  if(length(dots) > 0){
    predicates <- lapply(dots,  # NOTE: `map()` raises an error here
                         switch_expr_type_pred)
    # sometimes, because we call `map()`, we end up with predicates
    # buried one layer down in the list. Correct this
    if(length(predicates) == 1L){
      # if(is.list(predicates[[1]])){
      result <- predicates[[1]]
      # }
    }else{
      # wipe predicate names
      # NOTE: This step is *crucial*
      # without it, jsonlite::toJSON() wraps predicates in `{}` instead of `[]`
      # which is then rejected by GBIF
      names(predicates) <- NULL 
      result <- list(type = "and",
                     predicates = predicates)
    }
    as_predicates_filter(result)
  }else{
    NULL
  }
}

## -- Quosure parsing -- ##

#' Switch functions for quosures
#' @param x A (single) quosure
#' @noRd
#' @keywords internal
switch_expr_type_pred <- function(x, ...){
  switch(expr_type(x),
         "symbol" = {parse_symbol(x)}, # identical to `switch_expr_type()`
         "call" = {parse_call_pred(x, ...)}, # only 'new' line
         "literal" = {rlang::quo_get_expr(x)},
         cli::cli_abort("Quosure type not recognised.",
                        call = rlang::caller_env())
  )
}

#' Internal, recursive function to parse a call
#' 
#' Note that most entries passed to `filter` will be of type `call`, making this 
#' quite an important function. For example, `filter(y == 1)` is a call, but so 
#' are functions, e.g. `filter(list(x = 1))`.
#' 
#' Importantly, when using NSE, values are checked to see if they are present
#' in the working environment, but names are not, which is different to previous
#' galah behavior. So  `x <- 1; y <- 10; filter(y == x)` will parse to 
#' `list(y = 1)` not  `list(10 = 1)`. Advanced R suggests using `:=` for these 
#' cases, which could be added to `switch` below
#' @noRd
#' @keywords internal
parse_call_pred <- function(x){
  y <- rlang::quo_get_expr(x)
  env_tr <- rlang::quo_get_env(x)
  switch_lookup <- y[[1]] |>
    base::deparse() |>
    rlang::as_string() |>
    function_type()  # galah function
  switch(switch_lookup, # i.e. switch depending on what function is called
         "relational_operator" = parse_relational_pred(x),
         "logical_operator" = parse_logical_pred(x),
         "bracket" = parse_brackets_pred(x),
         "exclamation" = parse_exclamation_pred(x),
         "is.na" = parse_is_na_pred(x),
         "between" = parse_between_pred(x),
         "%in%" = parse_in_pred(x),
         rlang::eval_tidy(x) # if unknown, parse
         # {filter_error()} # if unknown, error
  )
}

#' Take standard filter-style queries and parse to `galah_filter()`-style `tibble`
#' Called by `parse_call`
#' @noRd
#' @keywords internal
parse_relational_pred <- function(x){

  # get expression
  expr <- rlang::quo_get_expr(x)
  if(length(expr) != 3L){
    filter_error()
  }
  
  # parse out separate parts
  operator <- as.character(expr[[1]])
  
  lhs <- rlang::f_lhs(expr) |> 
    rlang::as_label() |> 
    dequote() |> # galah function
    gbif_upper_case() # galah function
    
  rhs <- rlang::as_quosure(rlang::f_rhs(expr),
                           env = rlang::quo_get_env(x)) |>
    switch_expr_type_pred() |>
    as.character()
  
  # handle cases where someone passes a vector, i.e. year == c(2001, 2002)
  # these can be parse as 'in'
  if(length(rhs) > 1){
    list(
      type = "in",
      key = lhs,
      values = rhs)
    
  # otherwise we assume they are length-1 and continue
  }else{
    
    # 'does not equal' is handled hierarchically
    if(operator == "!="){
      list(
        type = "not",
        predicate = list(type = "equals",
                         key = lhs,
                         value = rhs))

    # everything else is flat
    }else{
      operator_text <- switch(operator,
                              "==" = "equals",
                              "<" = "lessThan",
                              "<=" = "lessThanOrEquals",
                              ">" = "greaterThan",
                              ">=" = "greaterThanOrEquals")
      list(type = operator_text,
           key = lhs,
           value = rhs)
    }
  }
}

#' Handle & and | statements
#' @noRd
#' @keywords internal
parse_logical_pred <- function(x){
  # get provided info
  provided_string <- rlang::quo_get_expr(x)[[1]] |> 
    rlang::as_string()
  
  # convert statements to strings
  if(grepl("\\|{1,2}", provided_string)){
    logical_string <- "or"
  }else{
    logical_string <- "and"
  }
  
  # wrap later predicates in supplied boolean
  # NOTE: deliberate use of `lapply()` and NOT `purrr::map()`
  subpredicates <- lapply(rlang::quo_get_expr(x)[-1], 
                          \(a){
                            rlang::as_quosure(a, 
                                              env = rlang::quo_get_env(x)) |>
                              switch_expr_type_pred()
                          })
  names(subpredicates) <- NULL
  list(type = logical_string,
       predicates = subpredicates)
}

#' Parse `call`s that contain brackets 
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @noRd
#' @keywords internal
parse_brackets_pred <- function(x){
  if(length(rlang::quo_get_expr(x)) != 2L){
    filter_error()
  }
  try_next_quosure_pred(x)
}

#' Parse `call`s that contain exclamations 
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @noRd
#' @keywords internal
parse_exclamation_pred <- function(x){
  next_section <- try_next_quosure_pred(x)
  next_length <- length(next_section)
  if(next_length == 2 & !is.null(next_section$type)){    
    if(next_section$type == "isNull"){
      next_section$type <- "isNotNull"
      next_section
    }else{
      list(type = "not", 
           predicate = next_section)      
    }
  }else{
    list(type = "not", 
         predicate = next_section)     
  }
}

#' A common pattern is to parse the first thing, then remove it and
#' keep going. This function is shorthand for that operation.
#' @noRd
#' @keywords internal
try_next_quosure_pred <- function(x){

  ## This version originally used in exclamation and brackets code
  rlang::as_quosure(rlang::quo_get_expr(x)[[-1]],
                    env = rlang::quo_get_env(x)) |>
    switch_expr_type_pred()
}

#' Parse `call`s that contain `is.na()`
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @noRd
#' @keywords internal
parse_is_na_pred <- function(x){
  if(length(rlang::quo_get_expr(x)) != 2L){
    filter_error()
  }

  variable <- rlang::as_label(rlang::quo_get_expr(x)[[2]]) |>
    dequote() |> # galah function
    gbif_upper_case() # galah function
  
  list(type = "isNull",
       parameter = variable)
}

#' Parse `call`s that contain `dplyr::between()`
#' Where this happens, they are always length-4, with "between" as the first entry.
#' @noRd
#' @keywords internal
parse_between_pred <- function(x){
  x_expr <- rlang::quo_get_expr(x)
  if(length(x_expr) < 4L){
    filter_error()
  }
  
  lhs <- x_expr[[2]] |>
    rlang::as_label() |> 
    dequote() |> # galah function
    gbif_upper_case() # galah function
    
  lower_bound <- list(type = "greaterThanOrEquals",
                      key = lhs,
                      value = rlang::as_label(x_expr[[3]]))
    
  upper_bound <- list(type = "lessThanOrEquals",
                      key = lhs,
                      value = rlang::as_label(x_expr[[4]]))
  
  list(type = "and",
       predicates = list(lower_bound, upper_bound))
}

#' Parse `call`s that contain `%in%`
#' 
#' Where this happens, they are always length-3, with "%in%" as the first entry.
#' @noRd
#' @keywords internal
parse_in_pred <- function(x){ 
  
  # extract relevant information
  lhs <- rlang::as_label(rlang::quo_get_expr(x)[[2]]) |>
    dequote() |> # galah function
    gbif_upper_case() # galah function
  
  rhs <- rlang::as_quosure(rlang::quo_get_expr(x)[[3]], 
                             env = rlang::quo_get_env(x)) |>
    switch_expr_type()
  ## NOTE: Not clear that this is correct. 
  ## Should parse to: `"values": ["cat1", "cat2", "cat3"]`
  
  # handle apostrophes (')
  if(any(stringr::str_detect(rhs, "\\'"))) {
    rhs <- gsub("'", "\\\\'", rhs)
  }
  
  # format as list
  list(
    type = "in",
    key = lhs,
    values = rhs)
}