#' Select filters to narrow down occurrence queries
#'
#' 'filters' are arguments of the form \code{field = value} that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (\code{year = 2020}), or records that are associated with a physical
#' specimen (\code{basisOfRecord = "PreservedSpecimen"}).
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
#' \code{\link{exclude}} for excluding a filter value.
#' @details
#' By default filters are included, but they can be excluded by wrapping the
#' filter values in \code{\link{exclude}} (see below for examples).
#' @examples \dontrun{
#' # Create a custom filter for records of interest
#' filters <- select_filters(
#'     basisOfRecord = "HumanObservation",
#'     year = 2020,
#'     stateProvince = "New South Wales")
#'
#' # Add the default ALA data quality profile
#' filters <- select_filters(
#'     basisOfRecord = "HumanObservation",
#'     year = 2020,
#'     stateProvince = "New South Wales",
#'     profile = "ALA")
#'     
#' # Use filters to exclude particular values
#' filters <- select_filters(year = exclude(seq(2011,2021)))
#' }
#' @export

# temporary version of select_filters
select_filters <- function(...){

  # abandoned options:
  # rlang::list2(...)  # returns named list of entries, fails with logical operators
  # rlang::enquo(...) # fails
  # substitute(...) # returns first arg only

  # return inputs as clean text
  call_initial <- deparse(sys.call())
  x <- gsub("^select_filters\\(|\\)$", "", call_initial)
  x <- gsub(",", " & ", x)
 
  # learn if there are any & or | statements, then extract them
  and_or_regex <- "\\&{1,2}|\\|{1,2}"
  logical_regex <- "(=|>|<|!)+"
  logical_join <- str_extract(x, and_or_regex)
  logical_join_parsed <- unlist(lapply(logical_join, parse_andor))
  
  # split by logical_lookup values, parse
  statements <- trimws(strsplit(x, and_or_regex)[[1]])
  df <- as.data.frame(do.call(rbind,
    lapply(strsplit(statements, logical_regex), trimws)))
  colnames(df) <- c("variable", "value")
  df$logical <- str_extract(statements, logical_regex)
  df <- df[, c(1, 3, 2)]
  
  # parse arguments or values
  df$variable <- parse_inputs(df$variable)
  df$value <- parse_inputs(df$value)
  
  # parse each line into a solr query
  df$query <- unlist(lapply(
    split(df, seq_len(nrow(df))),
    parse_logical
  ))
  
  # add join statments
  # note this fails with repeated OR statements
  df$join <- NA
  df$join[seq_len(nrow(df) - 1)] <- logical_join_parsed

  # set class etc
  class(df) <- append(class(df), "ala_filters")
  return(df)

}


parse_logical <- function(df){
  switch(df$logical,
    "=" = {paste0(df$variable, ":", df$value)},
    "==" = {paste0(df$variable, ":", df$value)},
    "!=" = {paste0("-", df$variable, ":", df$value)},
    ">=" = {paste0(df$variable, ":[", df$value, " TO *]")},
    ">" = {paste0(df$variable, ":[", df$value, " TO *] -", df$variable, "[", df$value, "]")},
    "<=" = {paste0(df$variable, ":[* TO ", df$value, "]")},
    "<" = {paste0(df$variable, ":[* TO ", df$value, "] AND -", df$variable, "[", df$value, "]")}
  )
}

parse_andor <- function(a){
  switch(a, 
    "|" = " OR ",
    "||" = " OR ",
    "&" = " AND ",
    "&&" = " AND "
  )
}

parse_inputs <- function(x){
  result <- x
  bracket_search <- grepl("\\(|\\[", x)
  if(any(bracket_search)){
    result[bracket_search] <- unlist(lapply(
      x[bracket_search],
      function(a){eval(str2lang(a))}
    ))
  }
  return(result)
}


## BELOW HERE IS IN THE ORIGINAL SELECT_FILTERS() CODE, 
## BUT IS NOT YET IMPLEMENTED ABOVE

# filters vs. fields terminology
# should handle miscased things?
# should try to fuzzy match?
# should also validate facets?
validate_filters <- function(filters) {
  # filters are provided in a dataframe
  # key should be a valid field name and value should be a valid category for
  # that field?
  invalid_filters <- names(filters)[!is.element(names(filters),
                                    c(search_fields()$id, all_fields()$name))]
  if (length(invalid_filters) > 0) {
    stop("The following filters are invalid: ",
         paste(invalid_filters, collapse = ", "),
         ". Use `search_fields()` to get a list of valid options")
  }
}

#' Negate a filter value
#' @rdname exclude
#' @param value string: filter value(s) to be excluded
#' @return value with class "exclude"
#' @seealso \code{exclude} is used with \code{\link{select_filters}} or
#' \code{\link{select_taxa}} to exclude values
#' @export exclude
exclude <- function(value) {
  class(value) <- c("exclude", class(value))
  value
}
