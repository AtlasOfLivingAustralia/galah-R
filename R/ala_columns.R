#' Build dataframe of columns to keep
#' @param group string: name of column group to include (either \code{'basic'}
#' or \code{'event'})
#' @param extra string: additional column names to return
#' @details
#' Calling the argument \code{group = 'basic'} returns the following columns:
#' \itemize{
#'   \item\code{decimalLatitude}
#'   \item\code{decimalLongitude}
#'   \item\code{eventDate}
#'   \item\code{taxon_name}
#'   \item\code{taxonConceptID}
#'   \item\code{recordID}
#'   \item\code{data_resource}
#' }
#' Conversely, using \code{group = 'event'} returns the following columns:
#' \itemize{
#'   \item\code{eventRemarks}
#'   \item\code{eventTime}
#'   \item\code{eventID}
#'   \item\code{eventDate}
#'   \item\code{samplingEffort}
#'   \item\code{samplingProtocol}
#' }
#' @export ala_columns
ala_columns <- function(group, extra) {
  if (!missing(group)) {
    group_cols <- data.table::rbindlist(lapply(group, function(x) {
      data.frame(name = preset_cols(x), type = "field",
                 stringsAsFactors = FALSE)
    }))} else {
      group_cols <- NULL
    }

  assertions <- ala_fields("assertion")$name
  if (!missing(extra)) {
    extra_cols <- data.table::rbindlist(lapply(extra, function(x) {
      type <- ifelse(x %in% assertions, "assertions", "field")
      data.frame(name = x, type = type, stringsAsFactors = FALSE)
    }))} else {
      extra_cols <- NULL
    }
  all_cols <- rbind(group_cols, extra_cols)
  # remove duplicates
  all_cols[!duplicated(all_cols$name), ]
}


preset_cols <- function(type) {
  valid_groups <- c("basic", "event")
  # use ALA version of taxon name to avoid ambiguity (2 fields map to dwc name)
  cols <- switch(type,
                 "basic" = c("decimalLatitude", "decimalLongitude",
                             "eventDate", "taxon_name", "taxonConceptID",
                             "recordID", "data_resource"),
                 "event" = c("eventRemarks", "eventTime", "eventID",
                             "eventDate", "samplingEffort",
                             "samplingProtocol"),
                 stop("\"", type,
                      "\" is not a valid column group. Valid groups are: ",
                      paste(valid_groups, collapse = ", "))
  )
  cols
}
