#' Select filters to narrow down occurrence queries
#' 
#' The result of `select_filters` can be passed to the `filters` argument in
#' \code{\link{ala_occurrences}} and \code{\link{ala_counts}}
#'
#' @param ... filters, in the form field = value
#' @param profile string: (optional) a data quality profile to apply to the
#' records. See \code{\link{find_profiles}} for valid profiles. By default
#' no profile is applied.
#' @return dataframe of filter values
#' @export select_filters

select_filters <- function(..., profile = NULL) {
  filters <- list(...)
  if (!is.null(profile)) {
    dq_filters <- find_profile_attributes(profile)
    dq_filter_rows <- data.table::rbindlist(lapply(dq_filters$filter,
                                                   function(filter) {
      split <- strsplit(filter, ":")[[1]]
      value <- str_replace_all(split[2], "\"", "")
      if (substr(split[1], 1, 1) == "-") {
        name <- substr(split[1], 2, nchar(split[1]))
        include <- FALSE
      } else {
        name <- split[1]
        include <- TRUE
      }
      data.frame(name = name, include, value = I(list(value)),
                 stringsAsFactors = FALSE)
    }))
  } else {
    dq_filter_rows <- NULL
  }
  
  
  assertions <- find_fields("assertion")$name
  validate_filters(filters)
  filter_rows <- data.table::rbindlist(lapply(names(filters), function(x) {
    if (x %in% assertions) {
      row <- data.frame(name = "assertions", include = TRUE, value = x,
                        stringsAsFactors = FALSE)
    } else {
      row <- data.frame(name = x, include = !inherits(filters[[x]], "exclude"),
                        value = I(list(filter_value(filters[[x]]))),
                        stringsAsFactors = FALSE)
    }
    row
  }))
  
  rbind(filter_rows, dq_filter_rows)
}


# filters vs. fields terminology
# should handle miscased things?
# should try to fuzzy match?
# should also validate facets?
validate_filters <- function(filters) {
  # filters are provided in a dataframe
  # key should be a valid field name and value should be a valid category for
  # that field
  # valid options is a combination of find_layers and find_fields?
  
  invalid_filters <- names(filters)[!names(filters) %in%
                                      c(find_fields()$name,
                                        "assertion", all_fields()$name)]
  if (length(invalid_filters) > 0) {
    stop("The following filters are invalid: ",
         paste(invalid_filters, collapse = ", "),
         ". Use `find_fields()` to get a list of valid options")
  }
}


# takes a dataframe and returns a built filter query
build_filter_query <- function(filters) {
  filters$name <- dwc_to_ala(filters$name)
  mapply(query_term, filters$name, filters$value, filters$include,
         USE.NAMES = FALSE)
}

query_term <- function(name, value, include) {
  # add quotes around value
  value <- lapply(value, function(x) {
    # don't add quotes if there are square brackets in the term
    if (grepl("\\[", x)) {
      x
    } else {
      paste0("\"", x, "\"")
    }
  })
  # add quotes around value
  if (include) {
    value_str <- paste0("(", paste(name, value, collapse = " OR ", sep = ":"),
                        ")")
  } else {
    value_str <- paste0("(", paste(paste0("-", name), value,
                                   collapse = ' AND ', sep = ":"), ")")
  }
  #paste0("(", value_str, ")")
  value_str
}


filter_value <- function(val) {
  # replace loigcal values with strings
  if (is.logical(val)) {
    return(ifelse(val, "true", "false"))
  }
  val
}

#' Negate a filter value
#' @param value string: filter value(s) to be excluded
#' @return value with class "exclude"
#' @export exclude
exclude <- function(value) {
  class(value) <- "exclude"
  value
}