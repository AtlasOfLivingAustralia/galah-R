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
  if(length(dots) > 0){
    check_named_input(dots)
    parsed_dots <- lapply(dots, switch_expr_type) 
    if(inherits(parsed_dots[[1]], "data_request")){
      list(data_request = parsed_dots[[1]],
           data = bind_rows(parsed_dots[-1]))
    }else{
      list(data = bind_rows(parsed_dots))
    }
  }else{
    NULL
  }
}

#' parse quosures, but for `select` and related functions
#' 
#' Major difference here is there is no need for parsing; simply return
#' stuff that is a named object
#' @noRd
#' @keywords internal
parse_quosures_basic <- function(dots){
  if(length(dots) > 0){
    parsed_dots <- lapply(dots, function(a){
      switch(expr_type(a),
            "symbol" = {parse_symbol(a)},
            "call" = {eval_tidy(a)},
            "literal" = {quo_get_expr(a)},
            abort("Quosure type not recognised"))
      })
    if(inherits(parsed_dots[[1]], "data_request")){
      list(data_request = parsed_dots[[1]], 
           data = unlist(parsed_dots[-1]))
    }else{
      list(data = unlist(parsed_dots))
    }
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
switch_expr_type <- function(x, ...){
  switch(expr_type(x),
         "symbol" = {parse_symbol(x)},
         "call" = {parse_call(x, ...)},
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
parse_call <- function(x, ...){
  y <- quo_get_expr(x)
  env_tr <- quo_get_env(x)
  
  switch(function_type(as_string(y[[1]])), # i.e. switch depending on what function is called
         "relational_operator" = parse_relational(y, env_tr, ...),
         "logical_operator" = parse_logical(y, env_tr, ...),
         "bracket" = parse_brackets(y, env_tr, ...),
         "exclamation" = parse_exclamation(y, env_tr),
         "is.na" = parse_is_na(y, env_tr, ...),
         "between" = parse_between(y, env_tr, ...),
         "%in%" = parse_in(y, env_tr, ...),
         "galah_call" = eval_tidy(x),
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
  }else if (x == "!"){
    "exclamation"
  }else if(x == "is.na"){
    "is.na"
  }else if(x == "between" | x == "dplyr::between"){
    "between"
  }else if(x == "%in%"){
    "%in%"
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
parse_relational <- function(expr, env, ...){
  if(length(expr) != 3L){filter_error()}
  # for LA cases
  result <- tibble(
    variable = as_label(expr[[2]]),
    logical = as.character(expr[[1]]), # should probably be `relational`
    value = as.character(switch_expr_type(as_quosure(expr[[3]], env = env))))
  
  # handle `!`
  dots <- list(...)
  if(!rlang::is_empty(dots) && result$logical == "==") {
    result$logical <- as.character("!=")
  }else if (!rlang::is_empty(dots) && result$logical == "!="){
    result$logical <- as.character("==")
    } else {
    result$logical <- result$logical
  }
  
  result$query <- parse_solr(result) # from `galah_filter.R`
  return(result)
}

#' Handle & and | statements
#' @importFrom rlang as_quosure expr_text
#' @noRd
#' @keywords internal
parse_logical <- function(expr, env, ...){
  provided_string <- as_string(expr[[1]])
  if(grepl("\\|{1,2}", provided_string)){
    logical_string <- "OR"
  }else{
    logical_string <- "AND"
  }
  
  linked_statements <- lapply(expr[-1], 
                       function(a){switch_expr_type(as_quosure(a, env = env), ...)}) 
  result <- linked_statements[[1]]
  # TODO: Something in this bit of code is broken...update 2023-15-06: might have been fixed
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
parse_brackets <- function(expr, env, ...){
  if(length(expr) != 2L){filter_error()}
  switch_expr_type(as_quosure(expr[[2]], env = env), ...) # pass this down the chain
}

#' Parse `call`s that contain exclamations 
#' 
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @importFrom rlang as_quosure
#' @noRd
#' @keywords internal
parse_exclamation <- function(expr, env){
  # extract call after `!`, preserves that `!` = TRUE
  switch_expr_type(as_quosure(expr[[2]], env = env), excl = TRUE) # pass this down the chain
}


#' Parse `call`s that contain `is.na()`
#' 
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @importFrom rlang as_quosure is_empty
#' @noRd
#' @keywords internal
parse_is_na <- function(expr, env, ...){
  
  # if(length(expr) != 2L){filter_error()}
  dots <- list(...)
  if(rlang::is_empty(dots)) {
    logical <- as.character("==")
  }else{
    logical <- as.character("!=")
  }
  # for LA cases
  result <- tibble(
    variable = switch_expr_type(as_quosure(expr[[2]], env = env)),
    logical = logical,
    value = as.character(""))
  result$query <- parse_solr(result)
  return(result)
}

#' Parse `call`s that contain `dplyr::between()`
#' 
#' Where this happens, they are always length-4, with "between" as the first entry.
#' @importFrom rlang as_quosure
#' @noRd
#' @keywords internal
parse_between <- function(expr, env, excl){ 
  if(length(expr) < 4L){filter_error()}
  # for LA cases
  if(isTRUE(excl)) {
    logical <- c(as.character(">"), c(as.character("<")))
  } else{
    logical <- c(as.character("<"), c(as.character(">")))
  }
  result <- tibble(
    variable = c(rep(as_label(expr[[2]]))),
    logical = logical,
    value = as.character(
      c(switch_expr_type(as_quosure(expr[[3]], env = env)),
        switch_expr_type(as_quosure(expr[[4]], env = env)))))
  result$query <- c(parse_solr(result[1,]), parse_solr(result[2,]))
  return(result)
}

#' Parse `call`s that contain `%in%`
#' 
#' Where this happens, they are always length-3, with "%in%" as the first entry.
#' @importFrom rlang as_quosure
#' @importFrom glue glue_collapse glue
#' @importFrom rlang parse_expr enquo
#' @noRd
#' @keywords internal
parse_in <- function(expr, env, excl){ 
  if(length(expr) < 3L){filter_error()}
  
  # convert to logical format using OR statements
  variable <- as_label(expr[[2]])
  
  if(missing(excl)) {
    logical <- "=="
  } else{
    logical <- "!="
  }
  
  value <- switch_expr_type(as_quosure(expr[[3]], env = env))
  
  in_as_or_statements <- rlang::parse_expr(
    glue::glue_collapse(
      glue("{variable} {logical} {value}"), 
      sep = " | "
    ))
  # in_as_or_statements_quos <- new_quosure(in_as_or_statements, env)
  parse_logical(enquo(in_as_or_statements), env) # pass this to parse_logical
  # class(quo_get_expr(in_as_or_statements_quos))
  # quo_is_call(rlang::enquo(in_as_or_statements))
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