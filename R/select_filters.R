#' Select filters to narrow down occurrence queries
#'
#' 'filters' are arguments of the form \code{field logical value} that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (\code{year = 2020}), or to return all records except for fossils
#'  (\code{basisOfRecord != "FossilSpecimen"}).
#' The result of \code{select_filters} can be passed to the \code{filters}
#' argument in \code{\link{ala_occurrences}()}, \code{\link{ala_species}()} or
#' \code{\link{ala_counts}()}.
#'
#' @param ... filters, in the form \code{field logical value}
#' @param profile \code{string}: (optional) a data quality profile to apply to the
#' records. See \code{\link{find_profiles}} for valid profiles. By default
#' no profile is applied.
#' @return A \code{data.frame} of filter values.
#' @seealso \code{\link{select_taxa}}, \code{\link{select_columns}} and
#' \code{\link{select_locations}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions. Use
#' \code{\link{search_fields}} to find fields that
#' you can filter by, and \code{\link{find_field_values}} to find what values
#' of those filters are available.
#' @details
#' All statements passed to \code{select_filters()} (except the \code{profile}
#' argument) take the form of field - logical - value. Permissible examples include:
#' \itemize{
#'   \item{\code{=} or \code{==} (e.g. \code{year = 2020})}
#'   \item{\code{!=}, e.g. \code{year != 2020})}
#'   \item{\code{>} or \code{>=} (e.g. \code{year >= 2020})}
#'   \item{\code{<} or \code{<=} (e.g. \code{year <= 2020})}
#'   \item{\code{OR} statements (e.g. \code{year == 2018 | year == 2020})}
#'   \item{\code{AND} statements (e.g. \code{year >= 2000 & year <= 2020})}
#' }
#' In some cases \code{R} will fail to parse inputs with a single equals sign 
#' (\code{=}), particularly where statements are separated by /code{&} or 
#' /code{|}. This problem can be avoided by using a double-equals instead.
#' @examples \dontrun{
#' # Create a custom filter for records of interest
#' filters <- select_filters(
#'     basisOfRecord = "HumanObservation",
#'     year >= 2010,
#'     stateProvince = "New South Wales")
#'
#' # Add the default ALA data quality profile
#' filters <- select_filters(
#'     basisOfRecord = "HumanObservation",
#'     year >= 2020,
#'     stateProvince = "New South Wales",
#'     profile = "ALA")
#'     
#' # Use filters to exclude particular values
#' select_filters(year >= 2010 & year != 2021)
#' 
#' # Separating statements with a comma is equivalent to an 'and' statement
#' select_filters(year >= 2010 & year < 2020) # is the same as:
#' select_filters(year >= 2010, year < 2020)
#' 
#' # All statements must include the field name
#' select_filters(year == 2010 | year == 2021) # this works (note double equals)
#' select_filters(year = c(2010, 2021)) # same as above 
#' select_filters(year == 2010 | 2021) # this fails
#'
#' # It is possible to use an object to specify required values
#' # numeric example
#' year_value <- 2010
#' select_filters(year > year_value)
#' # categorical example
#' basis_of_record <- c("HumanObservation", "MaterialSample")
#' select_filters(basisOfRecord = basis_of_record) 
#'
#' # solr supports range queries on text as well as numbers
#' select_filters(cl22 >= "Tasmania")
#' # queries all Australian States & Territories alphabetically after "Tasmania"
#' }
#' @export

# TODO: provide a useful error message for bad queries e.g. select_filters(year == 2010 | 2021)
# TODO: handle commas
select_filters <- function(..., profile = NULL) {
  exprs <- as.list(match.call(expand.dots = FALSE)$...)
  
  # sort out profiles
  profile_attr <- NULL
  if (!is.null(profile)) {
    short_name <- profile_short_name(profile)
    if (is.null(short_name) || is.na(short_name)) {
      stop("'", profile, "' is not a valid data quality id, short name or name.
      Use `find_profiles` to list valid profiles.")
    }
    profile_attr <- short_name
  }  
  
  # clean up user-provided objects
  # main job here is to parse objects that are passed unevaluated
  exprs <- clean_expressions(exprs)
  names(exprs) <- safe_eval(names(exprs))

  # then parse list entries as before
  df <- data.frame(data.table::rbindlist(lapply(
    seq_len(length(exprs)), 
    function(a){parse_filters(a, expr_list = exprs)})))

  # validate variables to ensure they exist in ALA
  if (getOption("galah_config")$run_checks) validate_filters(df$variable)
  # parse each line into a solr query
  if (nrow(df) == 0) {
    df <- data.frame(variable = character(),
                       logical = character(),
                       value = character(),
                       query = character())
  }
  # set class etc
  class(df) <- append(class(df), "ala_filters")
  attr(df, "dq_profile") <- profile_attr
  return(df)
}

# calls of the form `field = value`: split then evaluate
  # issue is that if a single, named object is supplied, we need to evaluate
  # but if a logical statement is supplied, we need to treat it as text
  # ergo we still need a section that detects two types of call and _does_ eval them
    # e.g. 1: `select_filters(paste(field, value, sep = " = "))`
    # e.g. 2: `object <- "field = value"; select_filters(object)`
  # next step is to detect objects from above examples and parse properly
clean_expressions <- function(exprs){
  lapply(exprs, function(x){
    if(is.call(x)){ # pre-character step necessary for calls
      x <- deparse(x)
    } 
    x <- as.character(x)  
    str_split <- strsplit(x, "\\s")[[1]]
    if(length(str_split) > 1){
      str_check <- !grepl("^[[:punct:]]+$", str_split)
      str_replace <- safe_eval(str_split[str_check])
      str_split[str_check] <- str_replace
      paste(str_split, collapse = " ")
    }else{
      safe_eval(x)
    }
  }) 
}

safe_eval <- function(x){ # where x is a vector
  unlist(lapply(x, function(a){ 
    if(is.null(a) | a == ""){
      ""
    }else{
      eval_try <- try(eval(parse(text = a)), silent = TRUE)
      if(inherits(eval_try, "try-error")){
        a
      }else{
        eval_try
      }
    }
  }))
}

parse_filters <- function(i, expr_list) {
  filter_name <- names(expr_list)[i]
  x <- expr_list[[i]]
  ## possibly no longer needed
  if(inherits(x, "name")){
    x <- eval(parse(text = x))
  }
  if(is.call(x)){
    x <- eval(x)
  }
  ## end no longer needed
  expr_type <- get_expr_type(x, filter_name)
  if (expr_type == "and_or") {
    x <- as.character(x)
    rows <- build_and_or_query(x)
  } else if (expr_type == "logical") {
    x <- as.character(x)
    rows <- build_logical_query(x)
  } else if (expr_type == "exclude") {
    x <- eval(x)
    rows <- data.frame(variable = filter_name,
                       logical = "!=",
                       value = x)
    rows$query <- parse_logical(rows)
  } else if (expr_type == "assertion") {
    logical <- ifelse(isTRUE(x), "=", "!=")
    rows <- data.frame(variable = "assertions",
                       logical = logical,
                       value = filter_name)
    rows$query <- parse_logical(rows)
  }
  else if (expr_type == "vector" || expr_type == "seq") {
    x <- eval(x)
    rows <- build_vector_query(filter_name, x, "=")
  } else {
    rows <- data.frame(variable = filter_name,
               logical = "=",
               value = eval(x))
    rows$query <- parse_logical(rows)
  }
  return(rows)
}


is_atomic <- function(x) {
  if (!grepl(and_or_regex(), x) & !grepl(logical_regex())) {
    return(TRUE)
  }
}

build_vector_query <- function(var, value, logical = "=") {
  rows <- data.frame(
    variable = var,
    logical = "=",
    value = paste(value, collapse = ",")
  )
  include <- ifelse(logical == "=", TRUE, FALSE)
  rows$query <- query_term(rows$variable, value, include)
  rows
}

build_and_or_query <- function(expr) {
  # this will convert the query to a character vector, in the process splitting
  # any and/or statements
  expr <- as.character(expr)
  # get the and/or character 
  and_or <- expr[grepl(and_or_regex(), expr)]
  statements <- expr[!grepl(and_or_regex(), expr)]
  if (length(and_or) > 1) {
    stop("Currently only one 'and/or' statement can be used per expression.")
  }
  rows <- as.data.frame(
    do.call(rbind,lapply(statements, build_logical_query))
  )
  # handle an or
  if (grepl("\\|", and_or)) {
    query <- paste0("(", paste(rows$query, collapse = " OR "), ")")
    rows$query <- query
  }
  rows
}

logical_regex <- function() { "(=|>|<|!)+" }
and_or_regex <- function() { "\\&{1,2}|\\|{1,2}" }

# Build a data.frame filter row from a logical statement
build_logical_query <- function(statement) {
  # was originally just a logical statement and has been split into 3
  if (length(statement) > 1) {
    logical <- statement[grepl(logical_regex(), statement)]
    components <- statement[!grepl(logical_regex(), statement)]
  } else {
   # logical statement might be given a full string
    logical <- str_extract(statement, "(=|>|<|!)+")
    components <- trimws(unlist(str_split(statement, "(=|>|<|!)+")))
  }
  value <- tryCatch(eval(parse(text = components[2])),
           error = function(e) {
             components[2]
           })
  rows <- data.frame(
    variable = components[1],
    logical = logical,
    # eval in case the user passed a variable here
    value = value
  )
  rows$query <- parse_logical(rows)
  rows
}

get_expr_type <- function(expr, filter_name) {
  expr <- as.character(expr)
  if (any(grepl(and_or_regex(), expr))) {
    return("and_or")
  } else if(any(grepl(logical_regex(), expr))) {
    return("logical")
  } else if ("exclude" %in% expr) {
    return("exclude")
  } else if (("c(" %in% expr) | length(expr) > 1) {
    return("vector")
  } else if ("seq(" %in% expr) {
    return("seq")
  } else if (filter_name %in% search_fields(type = "assertions")$id) {
    return("assertion")
  } else {
    return("character")
  }
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

# filters vs. fields terminology
# should handle miscased things?
# should try to fuzzy match?
# should also validate facets?
validate_filters <- function(values) {
  # filters are provided in a dataframe
  # key should be a valid field name and value should be a valid category for
  # that field?
  invalid_filters <- values[!is.element(values,
                                    c(search_fields()$id, all_fields()$name))]
  if (length(invalid_filters) > 0) {
    stop("The following filters are invalid: ",
         paste(invalid_filters, collapse = ", "),
         ". Use `search_fields()` to get a list of valid options")
  }
}

#' Negate a filter value
#'
#' Deprecated alternative to \code{select_filters(field != value)}.
#' @rdname exclude
#' @param value string: filter value(s) to be excluded
#' @return value with class "exclude"
#' @seealso \code{exclude} is used with \code{\link{select_filters}} or
#' \code{\link{select_taxa}} to exclude values
#' @export exclude
exclude <- function(value) {
  .Deprecated(msg = "`exclude` is deprecated as of galah v1.3. Please use != instead.")
  class(value) <- c("exclude", class(value))
  return(value)
}
