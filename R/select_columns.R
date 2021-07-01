#' Specify columns for occurrence download
#'
#' The ALA stores content on hundreds of different fields, and users often
#' thousands or millions of records at a time. To reduce time taken to download
#' data, and limit complexity of the resulting \code{data.frame}, it is often
#' sensible to restrict the columns returned by \code{\link{ala_occurrences}()}
#' to those that are most critical for a given application. This function allows
#' easy selection of individual columns, or commonly-requested groups of columns.
#' The resulting \code{data.frame} is then passed to the \code{columns}
#' argument in \code{\link{ala_occurrences}()}.
#'
#' @param ... zero or more individual column names to include
#' @param group \code{string}: (optional) name of one or more column groups to
#' include. Valid options are \code{'basic'}, \code{'event'} and
#' \code{'assertion'}
#' @return A \code{data.frame} of columns, specifying the name and type
#' of each column to include in the occurrence download.
#' @details
#' Calling the argument \code{group = 'basic'} returns the following columns:
#' \itemize{
#'   \item\code{decimalLatitude}
#'   \item\code{decimalLongitude}
#'   \item\code{eventDate}
#'   \item\code{scientificName}
#'   \item\code{taxonConceptID}
#'   \item\code{recordID}
#'   \item\code{dataResourceName}
#' }
#' Using \code{group = 'event'} returns the following columns:
#' \itemize{
#'   \item\code{eventRemarks}
#'   \item\code{eventTime}
#'   \item\code{eventID}
#'   \item\code{eventDate}
#'   \item\code{samplingEffort}
#'   \item\code{samplingProtocol}
#' }
#' Using \code{group = 'assertions'} returns all quality assertion-related
#' columns. The list of assertions is shown by \code{search_fields(type = "assertions")}.
#' @seealso \code{\link{select_taxa}}, \code{\link{select_filters}} and
#' \code{\link{select_locations}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions.
#' @export select_columns

select_columns <- function(..., group) {
  if (!missing(group)) {
    group_cols <- data.table::rbindlist(lapply(group, function(x) {
      type <- ifelse(x == "assertion", "assertions", "field")
      data.frame(name = preset_cols(x), type = type,
                 stringsAsFactors = FALSE)
    }))} else {
      group_cols <- NULL
    }

  assertions <- search_fields(type = "assertions")$id
  cols <- c(...)
  if (length(cols) > 0) {
    validate_cols(cols)
    extra_cols <- data.table::rbindlist(lapply(cols, function(x) {
      type <- ifelse(x %in% assertions, "assertions", "field")
      data.frame(name = x, type = type, stringsAsFactors = FALSE)
    }))} else {
      extra_cols <- NULL
    }
  all_cols <- rbind(group_cols, extra_cols)
  # remove duplicates
  all_cols[!duplicated(all_cols$name), ]
}

validate_cols <- function(cols) {
  invalid_cols <- cols[!is.element(cols,
                                   c(search_fields()$id, all_fields()$name))]
  if (length(invalid_cols) > 0) {
    message("The following columns may be invalid: ",
         paste(invalid_cols, collapse = ", "),
         ". Use `search_fields()` to get a list of valid options")
  }
}


preset_cols <- function(type) {
  valid_groups <- c("basic", "event", "assertions")
  cols <- switch(type,
                 "basic" = default_columns(),
                 "event" = c("eventRemarks", "eventTime", "eventID",
                             "eventDate", "samplingEffort",
                             "samplingProtocol"),
                 "assertions" = search_fields(type = "assertions")$id,
                 stop("\"", type,
                      "\" is not a valid column group. Valid groups are: ",
                      paste(valid_groups, collapse = ", "))
  )
  cols
}
