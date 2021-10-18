#' Specify fields for occurrence download
#'
#' The ALA stores content on hundreds of different fields, and users often
#' thousands or millions of records at a time. To reduce time taken to download
#' data, and limit complexity of the resulting \code{data.frame}, it is often
#' sensible to restrict the fields returned by \code{\link{ala_occurrences}()}
#' or when using the \code{group_by} argument of \code{\link{ala_counts}}.
#' This function allows easy selection of fields, or commonly-requested groups 
#' of columns.
#' 
#' @param ... zero or more individual column names to include
#' @param group \code{string}: (optional) name of one or more column groups to
#' include. Valid options are \code{"basic"}, \code{"event"} and
#' \code{"assertion"}
#' @param expand \code{logical}: When passed to \code{group_by} argument of 
#' \code{ala_counts}, should factor levels be expanded? Defaults to \code{FALSE}.
#' @return An object of class \code{data.frame} and \code{ala_fields}
#' specifying the name and type of each column to include in the 
#' call to \code{ala_counts} or |code{ala_occurrences}.
#' @details
#' Calling the argument \code{group = "basic"} returns the following columns:
#' \itemize{
#'   \item\code{decimalLatitude}
#'   \item\code{decimalLongitude}
#'   \item\code{eventDate}
#'   \item\code{scientificName}
#'   \item\code{taxonConceptID}
#'   \item\code{recordID}
#'   \item\code{dataResourceName}
#' }
#' Using \code{group = "event"} returns the following columns:
#' \itemize{
#'   \item\code{eventRemarks}
#'   \item\code{eventTime}
#'   \item\code{eventID}
#'   \item\code{eventDate}
#'   \item\code{samplingEffort}
#'   \item\code{samplingProtocol}
#' }
#' Using \code{group = "assertions"} returns all quality assertion-related
#' columns. The list of assertions is shown by \code{search_fields(type = "assertions")}.
#'
#' The \code{expand} argument is appended as an attribute of the \code{data.frame} 
#' returned by \code{select_fields}. It is only used by \code{ala_counts}, meaning
#' that setting \code{expand = TRUE} will not affect calls to \code{ala_occurrences}.
#' @note \code{select_columns} and \code{select_fields} are synonymous;
#' \code{select_columns} is deprecated and will be removed from future versions
#' of \code{galah}.
#' @seealso \code{\link{select_taxa}}, \code{\link{select_filters}} and
#' \code{\link{select_locations}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions; \code{\link{ala_counts}}
#' for how to get counts by levels of variables returned by \code{select_fields}.
#' @export

select_fields <- function(..., 
  group = c("basic", "event", "assertions"),
  expand = FALSE
) {
  if (!missing(group) && !is.null(group)) {
    group <- match.arg(group, several.ok = TRUE)
    group_cols <- data.table::rbindlist(lapply(group, function(x) {
      type <- ifelse(x == "assertions", "assertions", "field")
      data.frame(name = preset_cols(x), type = type,
                 stringsAsFactors = FALSE)
    }))} else {
      group_cols <- NULL
    }

  cols <- c(...)
  if (length(cols) > 0) {
    if (getOption("galah_config")$run_checks) validate_fields(cols)
    extra_cols <- data.table::rbindlist(lapply(cols, function(x) {
      type <- ifelse(str_detect(x, "[[:lower:]]"), "field", "assertions")
      data.frame(name = x, type = type, stringsAsFactors = FALSE)
    }))} else {
      extra_cols <- NULL
    }
  all_cols <- rbind(group_cols, extra_cols)
  if(inherits(all_cols, "data.table")){
    all_cols <- as.data.frame(all_cols)
  }
  class(all_cols) <- append(class(all_cols), "ala_fields")

  # remove duplicates
  all_cols[!duplicated(all_cols$name), ]
  
  # expand stuff
  if(nrow(all_cols) < 2){
    attr(all_cols, "expand") <- FALSE
  }else{
    attr(all_cols, "expand") <- expand
  }
  
  return(all_cols)
}

# To be deprecated
#' @rdname select_fields
#' @export
select_columns <- function(..., group, expand){
  select_fields(..., group, expand)
}

preset_cols <- function(type) {
  cols <- switch(type,
                 "basic" = default_columns(),
                 "event" = c("eventRemarks", "eventTime", "eventID",
                             "eventDate", "samplingEffort",
                             "samplingProtocol"),
                 "assertions" = search_fields(type = "assertions")$id)
  return(cols)
}
