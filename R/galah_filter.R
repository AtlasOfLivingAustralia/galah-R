#' Narrow a query by specifying filters
#'
#' 'filters' are arguments of the form `field logical value` that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (`year == 2020`), or to return all records except for fossils
#'  (`basisOfRecord != "FossilSpecimen"`).
#' The result of `galah_filter` can be passed to the `filters`
#' argument in [atlas_occurrences()], [atlas_species()] or
#' [atlas_counts()]. `galah_filter` uses non-standard evaluation (NSE),
#' and is designed to be as compatible as possible with `dplyr::filter`
#' syntax.
#'
#' @param ... filters, in the form `field logical value`
#' @param profile `string`: (optional) a data quality profile to apply to the
#' records. See [show_all_profiles()] for valid profiles. By default
#' no profile is applied.
#' @return An object of class `data.frame` and `galah_filter`,
#' containing filter values.
#' @seealso [search_taxa()] and [galah_geolocate()] for other ways to restrict 
#' the information returned by [atlas_occurrences()] and related functions. Use
#' [search_fields()] to find fields that
#' you can filter by, and [search_field_values()] to find what values
#' of those filters are available.
#' @details
#' All statements passed to `galah_filter()` (except the `profile`
#' argument) take the form of field - logical - value. Permissible examples include:
#'   * `=` or `==` (e.g. `year = 2020`)
#'   * `!=`, e.g. `year != 2020`)
#'   * `>` or `>=` (e.g. `year >= 2020`)
#'   * `<` or `<=` (e.g. `year <= 2020`)
#'   * `OR` statements (e.g. `year == 2018 | year == 2020`)
#'   * `AND` statements (e.g. `year >= 2000 & year <= 2020`)
#' 
#' In some cases `R` will fail to parse inputs with a single equals sign 
#' (`=`), particularly where statements are separated by `&` or 
#' `|`. This problem can be avoided by using a double-equals (`==`) instead.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Create a custom filter for records of interest
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' filters <- galah_filter(
#'     basisOfRecord == "HumanObservation",
#'     year >= 2010,
#'     stateProvince == "New South Wales")
#' ```
#'
#' Add the default ALA data quality profile
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' filters <- galah_filter(
#'     basisOfRecord == "HumanObservation",
#'     year >= 2020,
#'     stateProvince == "New South Wales",
#'     profile = "ALA")
#' ```
#'  
#' Use filters to exclude particular values
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' filter <- galah_filter(year >= 2010 & year != 2021)
#' 
#' atlas_counts(filter = filter)
#' ```
#' 
#' Separating statements with a comma is equivalent to an `AND` statement
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_filter(year >= 2010 & year < 2020) # is the same as:
#' galah_filter(year >= 2010, year < 2020)
#' ```
#' 
#' All statements must include the field name
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_filter(year == 2010 | year == 2021) # this works (note double equals)
#' galah_filter(year == c(2010, 2021)) # same as above 
#' galah_filter(year == 2010 | 2021) # this fails
#' ```
#'
#' It is possible to use an object to specify required values
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' # Numeric example
#' 
#' year_value <- 2010
#' 
#' galah_call() %>%
#'   galah_filter(year > year_value) %>%
#'   atlas_counts()
#' ```
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' # Categorical example
#' 
#' basis_of_record <- c("HumanObservation", "MaterialSample")
#' 
#' galah_call() %>%
#'   galah_filter(basisOfRecord == basis_of_record) %>%
#'   atlas_counts()
#' ```
#'
#' `solr` supports range queries on text as well as numbers. The following 
#' queries all Australian States and Territories alphabetically after "Tasmania"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() %>%
#'   galah_filter(cl22 >= "Tasmania") %>%
#'   atlas_counts()
#' ```
#' 
#' @importFrom rlang as_label  
#' @importFrom rlang caller_env         
#' @importFrom rlang enquos
#' @importFrom rlang eval_tidy
#' @importFrom rlang have_name
#' @importFrom rlang get_env
#' @importFrom rlang new_quosure
#' @importFrom rlang parse_expr
#' @importFrom rlang quo_get_expr
#' @export
  
galah_filter <- function(..., profile = NULL){
  
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
  
  # check to see if any of the inputs are a data request
  checked_dots <- detect_data_request(dots)
  if(!inherits(checked_dots, "quosures")){
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  }else{
    is_data_request <- FALSE
  }
 
  # Clean user arguments
  if(length(dots) > 0){
    
    # First, evaluate filters that use functions (if there are any)
    dots <- parse_objects_or_functions(dots)
    named_filters <- parse_inputs(dots)
    named_filters$query <- parse_query(named_filters)
    
    # Validate that variables exist in ALA
    if (getOption("galah_config")$run_checks) validate_fields(named_filters$variable)
    
  }else{ 
    # If no fields are entered, return an empty data frame of arguments
    named_filters <- data.frame(variable = character(),
                     logical = character(),
                     value = character(),
                     query = character())
  }
  
  # Set class
  named_filters <- as_tibble(named_filters)
  class(named_filters) <- append(class(named_filters), "galah_filter")
  
  # Check and apply profiles to query
  named_filters <- apply_profiles(profile, named_filters)
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, filter = named_filters)
  }else{
    named_filters
  }

}


# stop function to enforce new syntax, based on `dplyr` syntax
check_filter <- function(dots, error_call = caller_env()) {
  named <- have_name(dots)
  
  for (i in which(named)) {
    quo <- dots[[i]]
    
    # only allow named logical vectors, anything else
    # is suspicious
    expr <- quo_get_expr(quo)
    if (!is.logical(expr)) {
      name <- names(dots)[i]
      bullets <- c(
        "We detected a named input.",
        i = glue("This usually means that you've used `=` instead of `==`."),
        i = glue("Did you mean `{name} == {as_label(expr)}`?")
      )
      abort(bullets, call = error_call)
    }   
  }
}


# function to identify objects or functions in quosures, and eval them
# this is used twice; first to identify named objects passed to `galah_filter`,
# and again to parse variables and values for object status
parse_objects_or_functions <- function(dots){
  is_either <- is_function_check(dots) | is_object_check(dots)
  # If yes, evaluate them correctly as functions
  if(any(is_either)){
    index <- which(is_either)
    dots[index] <- lapply(dots[index], 
      function(a){eval_tidy(a) |> serialise_quosure()})
    }
  dots
}


# new function to clean up internals of parse_objects_or_functions
serialise_quosure <- function(x){
  if(length(x) > 1){
    new_quosure(
      paste0("c(", 
        paste(
          paste0("\"", x, "\""), # ensure values are quoted
          collapse = ", "
        ), 
      ")")
    )
  } else {
    new_quosure(x)
  }
}


# Catch-all function to do formula splitting etc
parse_inputs <- function(dots){
  
  x <- unlist(lapply(dots, as_label))
  env <- lapply(dots, get_env)
  
  x <- dequote(x)

  # remove problematic and/or statements
  x <- parse_or(unlist(x))
  x <- lapply(x, parse_and)

  # add section here to find index of parsed formulae
  index_df <- data.frame(
    index = rep(seq_along(x), lengths(x)),
    string = unlist(x)
  )
  x <- unlist(x)

  # parse formulae
  formula_regex <- "!=|>=|<=|==|>|<" # NOTE: order matters here
  # if '<' occurs before '<=', then '<=' is never detected

  formula_df <- data.frame(
    index = index_df$index,
    variable = sub(
      paste0("\\s*(", formula_regex, ")$"), "",
      str_extract(x, paste0("^.*(", formula_regex, ")"))
    ),
    logical = str_extract(x, formula_regex),
    value = sub(
      paste0("^(", formula_regex, ")\\s*"), "",
      str_extract(x, paste0("(", formula_regex, ").*$"))
    )
  )

  # the above returns NA when there are only single '=' (on purpose for consistency with dplyr)
  # remove values with NA rows
  formula_df <- formula_df[apply(formula_df[, 2:4], 1, function(a){!all(is.na(a))}), ]

  # check whether variables or values are named objects or functions, and return if they are
  var_quo <- lapply(
    seq_len(nrow(formula_df)),
    function(a){new_quosure(
      expr = parse_expr(formula_df$variable[a]), 
      env = env[[formula_df$index[a]]])})

  formula_df$variable <- dequote(unlist(
    lapply(parse_objects_or_functions(var_quo), as_label)))
  
  value_quo <- lapply(
    seq_len(nrow(formula_df)),
    function(a){new_quosure(
      expr = parse_expr(formula_df$value[a]), 
      env = env[[formula_df$index[a]]])})
    
  formula_df$value <- dequote(unlist(
    lapply(parse_objects_or_functions(value_quo), as_label)))

  # return minus the index field
  formula_df[, -1]
}


# remove quote marks
dequote <- function(x){
  quoting_regex <- grepl("(^\").+(\"$)", x)
  if(any(quoting_regex)){
    quotes_tr <- which(quoting_regex)
    x[quotes_tr] <- gsub("(^\")|(\"$)", "", x[quotes_tr])
  }
  x
}


# various parse functions
parse_and <- function(x){
  and_lookup <- grepl("&{1,2}", x)
  if(any(and_lookup)){
    x[and_lookup] <- strsplit(x[and_lookup], "&{1,2}")
    return(trimws(unlist(x)))
  }else{x}
}


parse_or <- function(x){
  or_lookup <- grepl("\\|{1,2}", x)
  if(any(or_lookup)){
    c_list <- lapply(
      strsplit(unlist(x[or_lookup]), "\\|{1,2}"),
      function(a){
        split_formulae <- strsplit(a, "(=|>|<|!)+")
        are_formulae <- lengths(split_formulae) == 2
        if(any(are_formulae)){
          split_formulae <- split_formulae[are_formulae]
          variable <- lapply(split_formulae, function(b){b[[1]]})[[1]]
          values <- paste0("c(", paste(
            trimws(unlist(
              lapply(split_formulae, function(b){b[[2]]})
            )), 
            collapse = ", "), ")")
          logical <- str_extract(a, "(=|>|<|!)+")[1]
          return(paste(variable, logical, values))
        }else{
          NULL
        }
    })
    x[or_lookup] <- unlist(c_list)
    x
  }else{
    x
  }
}  


parse_query <- function(df){

  # determine what 'type' of string it is
  df$type <- rep("logical", nrow(df))
  vector_check <- grepl("c\\(|seq\\(", df$value)
  if(any(vector_check)){
    df$type[vector_check] <- "vector"
  }
  assertion_check <- df$variable %in% show_all_fields(type = "assertions")$id
  if(any(assertion_check)){
    df$type[assertion_check] <- "assertion"
  }

  # build a valid solr query
  df$query <- unlist(lapply(
    split(df, seq_len(nrow(df))), 
    function(a){
      switch(a$type,
        "logical" = parse_logical(a),
        "vector" = parse_vector(a),
        "assertion" = parse_assertion(a)
      )
    }))

  # return query only
  return(df$query)
}


parse_logical <- function(df){
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


# question: does the below work when df$value is a character? May add 2x quotes
parse_vector <- function(df){
  clean_text <- gsub("\\\\", "", df$value) # remove multiple backslahses 
  values <- eval(parse(text = clean_text)) 
  paste0(
    if(df$logical == "!="){"-"},
    "(",
    paste(
      paste0(df$variable, ":\"", values, "\""),
    collapse = " OR "),
    ")")
}


parse_assertion <- function(df){
  logical <- isTRUE(as.logical(df$value))
  if(df$logical == "!="){logical <- !logical} # case where `variable != FALSE`
  logical_str <- ifelse(logical, "=", "!=")
  rows <- data.frame(variable = "assertions",
                     logical = logical_str,
                     value = df$variable)
  parse_logical(rows)
}


# ensure profiles are handled correctly
apply_profiles <- function(profile, named_filters, error_call = caller_env()) {
  profile_attr <- NULL
  if (!is.null(profile)) {
    short_name <- profile_short_name(profile)
    if (is.null(short_name) || is.na(short_name)) {
      bullets <- c(
        "Profile must be a valid name, short name, or data quality ID.",
        i = glue("Use `show_all_profiles()` to list valid profiles"),
        x = glue("'{profile}' is not recognised.")
      )
      abort(bullets, call = error_call)
    }
    profile_attr <- short_name
  }
  attr(named_filters, "dq_profile") <- profile_attr
  named_filters
}
