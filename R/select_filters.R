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
#' (\code{=}), particularly where statements are separated by \code{&} or 
#' \code{|}. This problem can be avoided by using a double-equals (\code{==}) instead.
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
# NOTE: help for ?parse says `call` is an order of magnitude faster
  # check if that's possible (perhaps with `do.call()`?) 

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
  df <- exprs |> 
    parse_named_entries() |>
    parse_class_name() |>
    parse_class_call() |>
    parse_formulae() |>
    unlist() |>
    parse_and() |>
    parse_or() |>
    parse_filters()
    
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

# NOTE: in the functions below, for-loop style lapply fails for some reason; 
# i.e. if you use lapply(exprs, function(x){inherits(a, "name")})
# then the resulting parsing is not passed back to the output
# hence the slightly odd index-based approach used below
  
# most basic approach is to have field = name; these give named entries
# parse these back to text for consistency with later functions
parse_named_entries <- function(x){
  name_lookup <- names(x) != ""
  if(any(name_lookup)){
    index <- which(name_lookup)
    x[index] <- paste(names(x)[index], x[index], sep = " = ")
    names(x) <- NULL
  }
  x
}

# where the object in list is of class `name` (i.e. is a named object),
# parse them, then paste back into original list
parse_class_name  <- function(x){
  class_name_lookup <- unlist(lapply(x, function(a){inherits(a, "name")}))
  if(any(class_name_lookup)){
    x[class_name_lookup] <- lapply(x[class_name_lookup], function(a){eval(a)})
  }
  x
}
  
# same but for objects of class `call`, which includes two types
  # function calls (e.g. paste(x, y)) that should be eval-ed
  # logical statements (e.g. year >= 2010) which shouldn't
parse_class_call <- function(x){
  class_call_lookup <- unlist(lapply(x, is.call))
  if(any(class_call_lookup)){
    call_text <- unlist(lapply(x[class_call_lookup], deparse))
    is_function <- grepl("([[:alnum:]]|.|_)+\\(", call_text)
    if(any(is_function)){
      x[class_call_lookup][is_function] <- lapply(
        x[class_call_lookup][is_function], function(a){eval(a)})
    }
    if(any(!is_function)){
      x[class_call_lookup][!is_function] <- call_text[!is_function]
    }
  }
  x
}
  
# by this point everything should be a character
# take any formulae and parse out fields and values
# the below is a bit convoluted, but should work ok as a block
parse_formulae <- function(x){
  formula_lookup <- unlist(lapply(x, function(a){grepl("=|>|<", a)}))
  formula_split <- strsplit(unlist(x[formula_lookup]), "\\s")
  # formulae contain:
    # > < = (ignore)
    # names of objects (parse)
    # names of fields (don't parse)
    # functions (e.g. c())
  all_vals <- unique(unlist(formula_split))
  all_vals <- all_vals[!grepl(">|<|=|\\(|\\)|\\||\\&", all_vals)]
  object_names <- all_vals[unlist(lapply(all_vals, exists))]
  object_list <- lapply(object_names, function(a){eval(parse(text = a))})
  names(object_list) <- object_names
  
  # where a vector is long, replace with c rather than original values
  length_lookup <- unlist(lapply(object_list, length)) > 1
  if(any(length_lookup)){
    object_list[length_lookup] <- lapply(
      object_list[length_lookup],
      function(a){paste0("c(", paste(a, collapse = ", "), ")")}) 
  }
  
  # use gsub via lapply to replace contents of exprs
  x_vec <- unlist(x)
  for(a in seq_along(object_list)){
    x_vec <- gsub(names(object_list)[a], object_list[[a]], x_vec)
  }
  
  return(as.list(x_vec))
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

parse_filters <- function(expr){
  # convert string a data.frame
  result_df <- as.data.frame(do.call(rbind, 
    strsplit(expr, "(=|>|<|!)+")))
  colnames(result_df) <- c("variable", "value")
  result_df$logical <- str_extract(expr, "(=|>|<|!)+")
  rownames(result_df) <- NULL
  result_df <- as.data.frame(lapply(result_df[, c(1, 3, 2)], trimws))
  
  # determine what 'type' of string it is
  result_df$type <- rep("logical", nrow(result_df))
  vector_check <- grepl("c\\(|seq\\(", result_df$value)
  if(any(vector_check)){
    result_df$type[vector_check] <- "vector"
  }
  assertion_check <- result_df$variable %in% search_fields(type = "assertions")$id
  if(any(assertion_check)){
    result_df$type[assertion_check] <- "assertion"
  }
  
  # build a valid solr query
  result_df$query <- unlist(lapply(
    split(result_df, seq_len(nrow(result_df))), 
    function(a){
      switch(a$type,
        "logical" = parse_logical(a),
        "vector" = parse_vector(a),
        "assertion" = parse_assertion(a)
      )
    }))
  
  # return everything except `type`
  return(result_df[, names(result_df) != "type"])
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