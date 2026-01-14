# Internal code to parse `...` info passed to `filter.data_request()` and 
# related functions. Note that the approach used below is taken from advanced R:
# https://adv-r.hadley.nz/expressions.html

## -- Top-level parsers -- ## 
# These are called 'first' by `galah_` functions

#' parse quosures for objects of class `data_request`
#' @noRd
#' @keywords internal
parse_quosures_data <- function(dots){ 
  if(length(dots) > 0){
    result <- purrr::map(dots, \(a){
      switch_expr_type(a)
      }) |>
      dplyr::bind_rows() |>
      clean_assertions() |>
      clean_logical_statements()
    result$query <- as.character(result$query)
  }else{
    result <- NULL
  }
  if(is.null(result)){
    result <- tibble::tibble(
      variable = character(),
      logical = character(),
      value = character(),
      query = character())
  }
  as_data_filter(result)
}
# FIXME: work out how to propagate `rlang::caller_env()` through the below functions

#' parse quosures, but for `select` and related functions
#' 
#' Major difference here is there is no need for parsing; simply return
#' stuff that is a named object
#' @noRd
#' @keywords internal
parse_quosures_basic <- function(dots,
                                 error_call = rlang::caller_env()){
  if(length(dots) > 0){
    parsed_dots <- purrr::map(dots, \(a){
      switch(expr_type(a),
             "symbol" = {parse_symbol(a)},
             "call" = {rlang::eval_tidy(a)},
             "literal" = {rlang::quo_get_expr(a)},
             cli::cli_abort("Quosure type not recognised.",
                            call = error_call))
    })
    unlist(parsed_dots)
  }else{
    NULL
  } 
}

#' parse quosures, but for `filter.files_request()` where we expect large amounts
#' of data to be supplied
#' @noRd
#' @keywords internal
parse_quosures_files <- function(dots,
                                 error_call = rlang::caller_env()){
  if(length(dots) > 0){
    check_named_input(dots)
    dot_expr <- rlang::quo_get_expr(dots[[1]]) # i.e. only first entry is available
    # get formula lhs
    lhs <- rlang::f_lhs(dot_expr)
    if(rlang::is_quosure(lhs)){
      lhs <- rlang::quo_get_expr(lhs)
    }
    lhs <- rlang::as_string(lhs) |> 
      dequote()
    # get rhs
    x <- rlang::f_rhs(dot_expr) |>
      rlang::new_quosure(env = rlang::quo_get_env(dots[[1]]))
    rhs <- switch(expr_type(x),
                  "call" = {rlang::eval_tidy(x)},
                  "symbol" = {if(exists(rlang::quo_get_expr(x), 
                                        where = rlang::quo_get_env(x))){
                    rlang::eval_tidy(x)
                  }else{
                    rlang::as_label(x)
                  }},
                  "literal" = {rlang::quo_get_expr(x)},
                  cli::cli_abort("Quosure type not recognised.", 
                                 call = error_call))
    # if(inherits(rhs, "data.frame")){
    list(variable = dequote(lhs), 
         data = rhs) |>
      as_files_filter() 
    # }else{
      # tibble::tibble(
      #   variable = dequote(lhs),
      #   logical = "==",
      #   value = rhs)
    # }
  }else{
    NULL
  }
}

## -- `data.frame` cleaning -- ##
# This is for cleaning the 'default' output from parsers

#' Function to ensure assertions are placed first in a query
#' This is important so that they are parsed correctly
#' Note this only gets triggered for AND statements - OR is handled earlier
#' @noRd
#' @keywords Internal
clean_assertions <- function(df){
  assertions_check <- grepl("assertions", df$variable)
  if(any(assertions_check)){
    check_1 <- concatenate_assertions(df[assertions_check, ], logical = "!=")
    check_2 <- concatenate_assertions(df[assertions_check, ], logical = "==")
    if(all(is.null(c(check_1, check_2)))){
      dplyr::bind_rows(
        df[assertions_check, ],
        df[!assertions_check, ]
      )
    }else{
      dplyr::bind_rows(
        check_1,
        check_2,
        df[!assertions_check, ]
      )      
    }
  }else{
    df
  }
}

#' Glue together AND-ed assertions
#' @noRd
#' @keywords Internal
concatenate_assertions <- function(df, logical){
    assertions_matched <- df$logical == logical
    if(length(which(assertions_matched)) > 1){
      df[assertions_matched, ] |>
        concatenate_logical_tibbles(provided_string = "&",
                                    logical_string = " AND ")
    }else{
      NULL
    }
}

#' Function to ensure OR statements parse correctly
#' @noRd
#' @keywords Internal
clean_logical_statements <- function(df){
  or_lookup <- 
    grepl("\\sOR\\s", df$query) & # OR statements
    !grepl("^-\\(", df$query)    # but not negative statements
  if(any(or_lookup)){
     # length(df$query) > 1){ # multiple OR + AND queries require ORs to have extra brackets
     # line above disabled as shown to cause issue #265
     # Problem here is that this test is strictly true; but `identify()` 
     # sometimes adds extra lines _after_ this is parsed, so the test can't be 
     # meaningfully applied within this function
    or_strings <- df$query[which(or_lookup)]
    df$query[which(or_lookup)] <- glue::glue("({or_strings})") |>
      as.character()
  }
  df
}

#' Internal function to remove quoting of variable names
#' Used as quotes are added when lhs is set using {{}}
#' @noRd
#' @keywords Internal
dequote <- function(x){
  gsub("^\"|\"$", "", x)
}


## -- Quosure parsing -- ##

#' Switch functions for quosures
#' @param x A (single) quosure
#' @noRd
#' @keywords internal
switch_expr_type <- function(x, ...){
  switch(expr_type(x),
         "symbol" = {parse_symbol(x)},
         "call" = {parse_call(x, ...)},
         "literal" = {rlang::quo_get_expr(x)},
         cli::cli_abort("Quosure type not recognised.")
  )
}

#' Get type from quosures
#' @param x A (single) quosure
#' @noRd
#' @keywords internal
expr_type <- function(x){
  if(rlang::quo_is_symbol(x)){
    "symbol"
  }else if(rlang::quo_is_call(x)){
    "call"
  }else if(rlang::quo_get_expr(x) |> rlang::is_syntactic_literal()){
    "literal"
  }else{
    typeof(x)
  }
}

#' Check whether symbols exist before they are parsed
#' @param x A (single) quosure
#' @noRd
#' @keywords internal
parse_symbol <- function(x){
  if(exists(rlang::quo_get_expr(x), where = rlang::quo_get_env(x))){
    result <- rlang::eval_tidy(x)
    if(inherits(result, "function")){ # special case for functions like 'data'
      rlang::as_label(x)              # which exist in Global
    }else{
      result
    }
  }else{
    rlang::as_label(x)
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
#' @noRd
#' @keywords internal
parse_call <- function(x, ...){
  y <- rlang::quo_get_expr(x)
  env_tr <- rlang::quo_get_env(x)
  switch_lookup <- y[[1]] |>
    deparse() |>
    rlang::as_string() |>
    function_type()
  switch(switch_lookup, # i.e. switch depending on what function is called
         "relational_operator" = parse_relational(x, ...),
         "logical_operator" = parse_logical(x, ...),
         "bracket" = parse_brackets(x, ...),
         "exclamation" = parse_exclamation(x),
         "is.na" = parse_is_na(x, ...),
         "between" = parse_between(x, ...),
         "%in%" = parse_in(x, ...),
         rlang::eval_tidy(x) # if unknown, parse
         # {filter_error()} # if unknown, error
  )
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
#' Called by `parse_call`
#' @noRd
#' @keywords internal
parse_relational <- function(x, ...){
  
  expr <- rlang::quo_get_expr(x)
  if(length(expr) != 3L){filter_error()}
  
  lhs <- rlang::f_lhs(expr) |> 
    rlang::as_label() |> 
    dequote()
  rhs <- rlang::as_quosure(rlang::f_rhs(expr), 
                           env = rlang::quo_get_env(x)) |>
    switch_expr_type() |>
    as.character()
  result <- tibble::tibble(
    variable = lhs,
    logical = as.character(expr[[1]]), # should probably be `relational`
    value = rhs)
  # handle `!`
  dots <- list(...)
  if("excl" %in% names(dots) && result$logical == "==") {
    result$logical <- as.character("!=")
  }else if ("excl" %in% names(dots) && result$logical == "!="){
    result$logical <- as.character("==")
  } else {
    result$logical <- result$logical
  }
  result$query <- parse_solr(result) # from `galah_filter.R`
  # add exception for nrow(result) > 1
  # this occurs when rhs is `c()`, which we interpret as "OR"
  if(nrow(result) > 1){
    result <- concatenate_logical_tibbles(result)
  }
  
  result
}

#' Handle & and | statements
#' @noRd
#' @keywords internal
parse_logical <- function(x, ...){
  provided_string <- rlang::quo_get_expr(x)[[1]] |> 
    rlang::as_string()
  if(grepl("\\|{1,2}", provided_string)){
    logical_string <- " OR "
  }else{
    logical_string <- " AND "
  }
  linked_statements <- purrr::map(as.list(rlang::quo_get_expr(x)[-1]), 
                                  \(a){
                                    a |>
                                      rlang::as_quosure(env = rlang::quo_get_env(x)) |>
                                      switch_expr_type(...)
                                  }) |> 
    dplyr::bind_rows()
  concatenate_logical_tibbles(linked_statements,
                              provided_string = provided_string,
                              logical_string = logical_string)
}

#' Internal function to handle concatenation of logicals
#' @noRd
#' @keywords Internal
concatenate_logical_tibbles <- function(df,
                                        provided_string = "|",
                                        logical_string = " OR "){
  if(all(df$variable == "assertions")){
    query_text <- df$query |>
      gsub("^-", "", x = _) |>
      glue::glue_collapse(sep = logical_string) 
    if(all(df$logical == "!=")){
      query_text <- glue::glue("-({query_text})")
    }
  }else{
    query_text <- df$query |>
      glue::glue_collapse(sep = logical_string)
  }
  tibble::tibble(
    variable = glue::glue_collapse(df$variable, sep = provided_string),
    logical  = glue::glue_collapse(df$logical,  sep = provided_string),
    value    = glue::glue_collapse(df$value,  sep = provided_string),
    query    = as.character(glue::glue("{query_text}")))
}

#' Parse `call`s that contain brackets 
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @noRd
#' @keywords internal
parse_brackets <- function(x, ...){
  if(length(rlang::quo_get_expr(x)) != 2L){
    filter_error()
  }
  rlang::quo_get_expr(x)[[-1]] |>
    rlang::as_quosure(env = rlang::quo_get_env(x)) |>
    switch_expr_type(...) # pass this down the chain
}

#' Parse `call`s that contain exclamations 
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @noRd
#' @keywords internal
parse_exclamation <- function(x){
  # extract call after `!`, preserves that `!` = TRUE
  rlang::quo_get_expr(x)[[-1]] |>
    rlang::as_quosure(env = rlang::quo_get_env(x)) |>
    switch_expr_type(excl = TRUE) # pass this down the chain
}

#' Parse `call`s that contain `is.na()`
#' Where this happens, they are always length-2, with "(" as the first entry.
#' @noRd
#' @keywords internal
parse_is_na <- function(x, ...){
  if(length(rlang::quo_get_expr(x)) != 2L){
    filter_error()
  }
  dots <- list(...)
  logical <- ifelse(rlang::is_empty(dots), "==", "!=")
  # for LA cases
  result <- tibble::tibble(
    variable = rlang::quo_get_expr(x)[[2]] |>
      rlang::as_quosure(env = rlang::quo_get_env(x)) |>
      switch_expr_type(),
    logical = logical,
    value = as.character(""))
  result$query <- parse_solr(result)
  return(result)
}

#' Parse `call`s that contain `dplyr::between()`
#' Where this happens, they are always length-4, with "between" as the first entry.
#' @noRd
#' @keywords internal
parse_between <- function(x, excl){ 
  if(length(rlang::quo_get_expr(x)) < 4L){
    filter_error()
  }
  # for LA cases
  if(isTRUE(excl)) {
    logical <- c(as.character(">"), c(as.character("<")))
  } else{
    logical <- c(as.character("<"), c(as.character(">")))
  }
  result <- tibble::tibble(
    variable = c(rep(rlang::as_label(rlang::quo_get_expr(x)[[2]]))),
    logical = logical,
    value = as.character(
      c(switch_expr_type(rlang::as_quosure(rlang::quo_get_expr(x)[[3]], 
                                           env = rlang::quo_get_env(x))),
        switch_expr_type(rlang::as_quosure(rlang::quo_get_expr(x)[[4]], 
                                           env = rlang::quo_get_env(x))))))
  result$query <- c(parse_solr(result[1,]), 
                    parse_solr(result[2,]))
  return(result)
}

#' Parse `call`s that contain `%in%`
#' 
#' Where this happens, they are always length-3, with "%in%" as the first entry.
#' @noRd
#' @keywords internal
parse_in <- function(x, excl){ 
  # convert to logical format using OR statements
  variable <- rlang::quo_get_expr(x)[[2]] |>
    rlang::as_label()
  logical <- ifelse(missing(excl), "==", "!=")
  value <- rlang::quo_get_expr(x)[[3]] |>
    rlang::as_quosure(env = rlang::quo_get_env(x)) |>
    switch_expr_type()
  # handle apostrophes (')
  if(any(stringr::str_detect(value, "\\'"))) {
    value <- gsub("'", "\\\\'", value)
  }
  # convert to formula
  in_as_or_statements <- glue::glue_collapse(
    glue::glue("{variable} {logical} '{value}'"), 
    sep = " | "
  ) |>
    rlang::parse_expr()
  # convert to quosure and pass to `parse_logical()`
  rlang::as_quosure(x = in_as_or_statements, 
                    env = rlang::quo_get_env(x)) |>
    parse_logical()
}

#' Parse `call`s that contain `c()`
#' Where this happens, they are always length-2, with "c()" as the first entry.
#' @noRd
#' @keywords internal
parse_c <- function(x, excl){ 
  if(length(quo_get_expr(x)) < 2L){
    filter_error()
  }
  # convert to logical format using OR statements
  variable <- rlang::quo_get_expr(x)[[1]] |>
    rlang::as_label()
  logical <- ifelse(missing(excl), "==", "!=")
  value <- rlang::quo_get_expr(x)[[3]] |>
    rlang::as_quosure(env = rlang::quo_get_env(x)) |>
    switch_expr_type()
  in_as_or_statements <- glue::glue_collapse(
    glue::glue("{variable} {logical} '{value}'"), 
    sep = " | ") |>
    rlang::parse_expr()
  parse_logical(rlang::enquo(in_as_or_statements), 
                rlang::quo_get_env(x)) # pass this to parse_logical
}


## -- SOLR conversion -- ##

#' Convert information in a `tibble` to a `solr` query
#' Previously `galah_filter.R/parse_logical`, but altered to support multi-row tibbles
#' @noRd
#' @keywords internal
parse_solr <- function(df){
  if(nrow(df) > 1){
    purrr::map(
      split(df, seq_len(nrow(df))),
      switch_solr) |>
    unlist()
  }else{
    switch_solr(df)
  }
}

#' Internal function to `parse_solr()`
#' @noRd
#' @keywords internal
switch_solr <- function(df){
  switch(df$logical,
         "==" = query_term(df$variable, df$value, TRUE),
         "!=" = query_term(df$variable, df$value, FALSE),
         ">=" = glue::glue_data(df, "{variable}:[{value} TO *]"),
         ">"  = {
           lowest_value <- query_term(df$variable, df$value, TRUE)
           glue::glue_data(df, "{variable}:[{value} TO *] AND -{lowest_value}")},
         "<=" = glue::glue_data(df, "{variable}:[* TO {value}]"),
         "<"  = {
           highest_value <- query_term(df$variable, df$value, TRUE)
           glue::glue_data(df, "{variable}:[* TO {value}] AND -{highest_value}")}
  )
}

#' Generic error for unknown cases
#' @noRd
#' @keywords internal
filter_error <- function(){
  cli::cli_abort("Invalid argument passed to `filter()`.",
                 call = rlang::caller_env())
}

#' Subfunction called by `parse_solr()`
#' @noRd
#' @keywords internal
query_term <- function(name, value, include) {
  # add quotes around value
  value <- purrr::map(value, rlang::expr_text) # format query value as solr-readable text
  if(value %in% c("", "\"\"")) {
    if(include){
      value_str <- glue::glue("(*:* AND -{name}:*)")  # queries with "=="
    }else{
      value_str <- glue::glue("({name}:*)") # queries with "!="
    }
  } else {
    # assertions do not require brackets
    if(name == "assertions"){
      value_str <- glue::glue("{name}:{value}")
    }else{
      value_str <- glue::glue("({name}:{value})")
    }
    # negations have a leading `-`
    if(!include){
      value_str <- glue::glue("-{value_str}")
    }
  }
  value_str
}