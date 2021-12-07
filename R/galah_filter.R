#' Choose filters to narrow down occurrence queries
#'
#' 'filters' are arguments of the form \code{field logical value} that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (\code{year == 2020}), or to return all records except for fossils
#'  (\code{basisOfRecord != "FossilSpecimen"}).
#' The result of \code{galah_filter} can be passed to the \code{filters}
#' argument in \code{\link{ala_occurrences}()}, \code{\link{ala_species}()} or
#' \code{\link{ala_counts}()}. \code{galah_filter} uses non-standard evaluation (NSE),
#' and is designed to be as compatible as possible with \code{dplyr::filter}
#' syntax.
#'
#' @param ... filters, in the form \code{field logical value}
#' @param profile \code{string}: (optional) a data quality profile to apply to the
#' records. See \code{\link{find_profiles}} for valid profiles. By default
#' no profile is applied.
#' @return An object of class \code{data.frame} and \code{galah_filter},
#' containing filter values.
#' @seealso \code{\link{select_taxa}}, \code{\link{select_columns}} and
#' \code{\link{galah_location}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions. Use
#' \code{\link{search_fields}} to find fields that
#' you can filter by, and \code{\link{find_field_values}} to find what values
#' of those filters are available.
#' @details
#' All statements passed to \code{galah_filter()} (except the \code{profile}
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
#' (\code{=}), particularly where statements are separated by \code{&} or 
#' \code{|}. This problem can be avoided by using a double-equals (\code{==}) instead.
#' @examples \dontrun{
#' # Create a custom filter for records of interest
#' filters <- galah_filter(
#'     basisOfRecord == "HumanObservation",
#'     year >= 2010,
#'     stateProvince == "New South Wales")
#'
#' # Add the default ALA data quality profile
#' filters <- galah_filter(
#'     basisOfRecord == "HumanObservation",
#'     year >= 2020,
#'     stateProvince == "New South Wales",
#'     profile = "ALA")
#'     
#' # Use filters to exclude particular values
#' galah_filter(year >= 2010 & year != 2021)
#' 
#' # Separating statements with a comma is equivalent to an 'and' statement
#' galah_filter(year >= 2010 & year < 2020) # is the same as:
#' galah_filter(year >= 2010, year < 2020)
#' 
#' # All statements must include the field name
#' galah_filter(year == 2010 | year == 2021) # this works (note double equals)
#' galah_filter(year == c(2010, 2021)) # same as above 
#' galah_filter(year == 2010 | 2021) # this fails
#'
#' # It is possible to use an object to specify required values
#' # numeric example
#' year_value <- 2010
#' galah_filter(year > year_value)
#' # categorical example
#' basis_of_record <- c("HumanObservation", "MaterialSample")
#' galah_filter(basisOfRecord == basis_of_record) 
#'
#' # solr supports range queries on text as well as numbers
#' galah_filter(cl22 >= "Tasmania")
#' # queries all Australian States & Territories alphabetically after "Tasmania"
#' }
#' @importFrom rlang enquos as_label get_env eval_tidy new_quosure
#' @export

# TODO: provide a useful error message for bad queries e.g. galah_filter(year == 2010 | 2021)
# TODO: handle commas
# NOTE: help for ?parse says `call` is an order of magnitude faster
  # check if that's possible (perhaps with `do.call()`?) 
  
galah_filter <- function(..., profile = NULL){
  
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
 
  # The following code cleans user arguments
  if(length(dots) > 0){
    
    # First, parse named objects by using quosure directly
    is_function_call <- is_function_grepl(lapply(dots, as_label))
    if(any(is_function_call)){
      dots[which(is_function_call)] <- lapply(
        dots[which(is_function_call)], 
        function(a){new_quosure(eval_tidy(a))
      })
    }
    
    # Second, parse user-supplied filters to build ALA query
    exprs <- unlist(lapply(dots, as_label)) # This returns text that you can 
                                            # check with regex
    # alternative is `unlist(lapply(dots, get_expr))` which returns calls etc
    environments <- lapply(dots, get_env)  
    named_filters <- parse_inputs(exprs, environments)
    named_filters$query <- parse_query(named_filters)
    
    # Validate that variables exist in ALA
    if (getOption("galah_config")$run_checks) validate_fields(named_filters$variable)
    
  }else{ 
    # If no fields are given, return an empty data frame of arguments
    named_filters <- data.frame(variable = character(),
                     logical = character(),
                     value = character(),
                     query = character())
  }
  
  # Set class
  class(named_filters) <- append(class(named_filters), "galah_filter")
  
  # Check and apply profiles to query
  profile_attr <- NULL
  if (!is.null(profile)) {
    short_name <- profile_short_name(profile)
    if (is.null(short_name) || is.na(short_name)) {
      bullets <- c(
        "Profile must be a valid name, short name, or data quality ID.",
        i = glue::glue("Use `find_profiles()` to list valid profiles"),
        x = glue::glue("'{profile}' is not recognised.")
      )
      rlang::abort(bullets, call = rlang::caller_env())
    }
    profile_attr <- short_name
  }  
  attr(named_filters, "dq_profile") <- profile_attr
  
  named_filters
}


check_filter <- function(dots) {
  named <- rlang::have_name(dots)
  
  for (i in which(named)) {
    quo <- dots[[i]]
    
    # only allow named logical vectors, anything else
    # is suspicious
    expr <- rlang::quo_get_expr(quo)
    if (!is.logical(expr)) {
      name <- names(dots)[i]
      bullets <- c(
        "We detected a named input.",
        i = glue::glue("This usually means that you've used `=` instead of `==`."),
        i = glue::glue("Did you mean `{name} == {as_label(expr)}`?")
      )
      rlang::abort(bullets, call = rlang::caller_env())
    }
    
  }
}

# Catch-all function to do formula splitting etc
parse_inputs <- function(x, env){
  
  # remove quote marks
  quoting_regex <- grepl("(^\").+(\"$)", x)
  if(any(quoting_regex)){
    quotes_tr <- which(quoting_regex)
    x[quotes_tr] <- gsub("(^\")|(\"$)", "", x[quotes_tr])
  }

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
  formula_regex <- ">|<|>=|<=|=="

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

  formula_df$variable <- parse_objects(formula_df$variable, env[formula_df$index])
  formula_df$value <- parse_objects(formula_df$value, env[formula_df$index])

  formula_df[, -1]
}


parse_objects <- function(x, env){
  is_object <- unlist(lapply(
   seq_along(x),
   function(a){
     exists(x[[a]], envir = env[[a]]) &
     !exists(x[[a]], envir = env[[a]], mode = "function") 
   }))
  is_function_call <- is_function_grepl(x) 
  is_either <- is_object | is_function_call

  if(any(is_either)){

    # parse out objects that have been discovered  
    index <- which(is_either)
    object_list <- lapply(index, function(a){
     eval(parse(text = x[[a]]), envir = env[[a]]) 
    })

    # where a vector is long, replace with c rather than original values
    length_lookup <- lengths(object_list) > 1
    if(any(length_lookup)){
     object_list[length_lookup] <- lapply(
       object_list[length_lookup],
       function(a){paste0("c(", paste(a, collapse = ", "), ")")}) 
    }  

    # output replacement
    x[index] <- unlist(object_list)
   
  }
  x
}


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
      strsplit(x[or_lookup], "\\|{1,2}"),
      function(a){
        split_formulae <- strsplit(a, "(=|>|<|!)+")
        variable <- lapply(split_formulae, function(b){b[[1]]})[[1]]
        values <- paste0("c(", paste(
          trimws(unlist(
            lapply(split_formulae, function(b){b[[2]]})
          )), 
          collapse = ", "), ")")
        logical <- str_extract(a, "(=|>|<|!)+")[1]
        return(paste(variable, logical, values))
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
  assertion_check <- df$variable %in% search_fields(type = "assertions")$id
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


# question: does the below work when df$value is a characater? May add 2x quotes
parse_vector <- function(df){
  values <- eval(parse(text = df$value))
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


is_function_grepl <- function(x){
  if(is.list(x)){x <- unlist(x)}
  (
    (grepl("^(([[:alnum:]]|\\.|_)+\\()", x) & grepl("\\)", x)) |
    grepl("\\$|\\[", x) 
  ) & !grepl( ">|<|>=|<=|==", x)
}

