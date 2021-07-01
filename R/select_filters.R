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
#' @param ... filters, in the form \code{field = value}
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
#' @export select_filters
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

select_filters <- function(..., profile = NULL) {
  filters <- list(...)
  if (!is.null(profile)) {
    short_name <- profile_short_name(profile)
    if (is.null(short_name) || is.na(short_name)) {
      stop(profile, " is not a valid data quality id, short name or name. Use
          `find_profiles` to list valid profiles.")
    }
    dq_filter_row <- data.frame(name = "profile", include = TRUE, value = I(list(short_name)),
                 stringsAsFactors = FALSE)
  } else {
    dq_filter_row <- NULL
  }

  assertions <- search_fields(type = "assertions")$id
  validate_filters(filters)
  filter_rows <- data.table::rbindlist(lapply(names(filters), function(x) {
    if (x %in% assertions) {
      row <- data.frame(name = "assertions", include = filters[[x]], value = I(list(x)),
                        stringsAsFactors = FALSE)
    } else {
      row <- data.frame(name = x, include = !inherits(filters[[x]], "exclude"),
                        value = I(list(filter_value(filters[[x]]))),
                        stringsAsFactors = FALSE)
    }
    row
  }))
  
  rbind(filter_rows, dq_filter_row)
}


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