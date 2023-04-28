# Internal code to parse `...` info passed to `filter.data_request()`
# Note that the approach used below is taken from advanced R:
# https://adv-r.hadley.nz/expressions.html
# It is adapted from code written in `potions`:
# https://www.github.com/atlasOfLivingAustralia/potions

#' parse_quosures
#' @noRd
#' @importFrom dplyr bind_rows
#' @importFrom rlang abort
#' @importFrom rlang quo_is_symbol
#' @keywords internal
parse_quosures <- function(dots){
  name_length <- any(length(names(dots) > 0)) & any(names(dots) != "")
  if(name_length){
    bullets <- c(
      "We detected a named input.",
      i = "This usually means that you've used `=` instead of `==`.")
    abort(bullets)
  }
  if(length(dots) > 0){
    lapply(dots, switch_expr_type) |>
      bind_rows()
  }else{
    NULL
  }
}

#' Switch functions for quosures
#' @param x A (single) quosure
#' @importFrom rlang abort
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @noRd
#' @keywords internal
switch_expr_type <- function(x){
  switch(expr_type(x),
         "symbol" = {parse_symbol(x)},
         "call" = {parse_call(x)},
         "literal" = {quo_get_expr(x)},
         abort("Quosure type not recognised")
  )
}

#' Get type from quosures
#' @param x A (single) quosure
#' @importFrom rlang quo_is_symbol
#' @importFrom rlang quo_is_call
#' @importFrom rlang quo_get_expr
#' @importFrom rlang is_syntactic_literal
#' @noRd
#' @keywords internal
expr_type <- function(x){
  if(quo_is_symbol(x)){
    "symbol"
  }else if(quo_is_call(x)){
    "call"
  }else if(is_syntactic_literal(quo_get_expr(x))){
    "literal"
  }else{
    typeof(x)
  }
}

#' Check whether symbols exist before they are parsed
#' @param x A (single) quosure
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @importFrom rlang eval_tidy
#' @importFrom rlang as_label
#' @noRd
#' @keywords internal
parse_symbol <- function(x){
  if(exists(quo_get_expr(x), where = quo_get_env(x))){
    result <- eval_tidy(x)
    if(inherits(result, "function")){ # special case for functions like 'data'
      as_label(x)                     # which exist in Global
    }else{
      result
    }
  }else{
    as_label(x)
  }
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
#' @importFrom rlang abort
#' @importFrom rlang as_quosure
#' @importFrom rlang as_string
#' @importFrom rlang eval_tidy
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @noRd
#' @keywords internal
parse_call <- function(x){
  y <- quo_get_expr(x)
  env_tr <- quo_get_env(x)
  switch(function_type(as_string(y[[1]])), # i.e. switch depending on what function is called
         "relational_operator" = parse_relational(y, env_tr),
         "logical_operator" = parse_logical(y, env_tr),
         "bracket" = parse_brackets(y, env_tr),
         {filter_error()}) # if unknown, error
}

#' Determine the 'type' of call in `parse_call`, using the first entry of the AST
#' @noRd
#' @keywords internal
function_type <- function(x){ # assumes x is a string
  if(grepl("!=|>=|<=|==|>|<", x)){ # see ?`==`. Note: ignores `:=`
    "relational_operator" 
  }else if(grepl("\\&{1,2}|\\|{1,2}", x)){ # see ?`|`
    "logical_operator" 
  }else if(x == "("){
    "bracket"
  }else{
    x
  }
}

#' Take standard filter-style queries and parse to `galah_filter()`-style `tibble`
#' 
#' Called by `parse_call`
#' @importFrom tibble tibble
#' @importFrom rlang as_label
#' @importFrom rlang as_quosure
#' @noRd
#' @keywords internal
parse_relational <- function(expr, env){
  if(length(expr) != 3L){filter_error()}
  # for LA cases
  result <- tibble(
    variable = as_label(expr[[2]]),
    logical = as.character(expr[[1]]), # should probably be `relational`
    value = as.character(switch_expr_type(as_quosure(expr[[3]], env = env))))
  result$query <- parse_solr(result) # from `galah_filter.R`
  return(result)
}

#' Handle & and | statements
#' @importFrom rlang as_quosure
#' @noRd
#' @keywords internal
parse_logical <- function(expr, env){
  provided_string <- as_string(expr[[1]])
  if(grepl("\\|{1,2}", provided_string)){
    logical_string <- "OR"
  }else{
    logical_string <- "AND"
  }
  linked_statements <- lapply(expr[-1], 
                       function(a){as_quosure(a, env = env) |>
                                   switch_expr_type()}) 
  result <- linked_statements[[1]]
  result$variable <- join_logical_strings(linked_statements, "variable", provided_string)
  result$logical <- join_logical_strings(linked_statements, "logical", provided_string)
  result$value <- join_logical_strings(linked_statements, "value", provided_string)
  result$query <- join_logical_strings(linked_statements, "query", logical_string)
  result$query <- paste0("(", result$query, ")")
  return(result)
}

#' Messy internal function called by `parse_logical`
#' @noRd
#' @keywords internal
join_logical_strings <- function(x, variable, collapse){
  lapply(x, function(a){a[[variable]]}) |> 
    unlist() |>
    paste(collapse = collapse)
}

#' Parse `call`s that contain brackets 
#' 
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @importFrom rlang as_quosure
#' @noRd
#' @keywords internal
parse_brackets <- function(expr, env){
  if(length(expr) != 2L){filter_error()}
  switch_expr_type(as_quosure(expr[[2]], env = env)) # pass this down the chain
}

#' Convert information in a `tibble` to a `solr` query
#'
#' Previously `galah_filter.R/parse_logical`
#' @noRd
#' @keywords internal
parse_solr <- function(df){
  switch(df$logical,
         "=" = {query_term(df$variable, df$value, TRUE)},
         "==" = {query_term(df$variable, df$value, TRUE)},
         "!=" = {query_term(df$variable, df$value, FALSE)},
         ">=" = {paste0(df$variable, ":[", df$value, " TO *]")},
         ">" = {paste0(df$variable, ":[", df$value, " TO *] AND -", query_term(df$variable, df$value, TRUE))},
         "<=" = {paste0(df$variable, ":[* TO ", df$value, "]")},
         "<" = {paste0(df$variable, ":[* TO ", df$value, "] AND -", query_term(df$variable, df$value, TRUE))}
  )
}

#' Generic error for unknown cases
#' @importFrom rlang abort
#' @noRd
#' @keywords internal
filter_error <- function(){abort("Invalid argument passed to `filter()`")}