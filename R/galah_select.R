#' Specify fields for occurrence download
#'
#' The living atlases store content on hundreds of different fields, and users
#' often require thousands or millions of records at a time. To reduce time taken
#' to download data, and limit complexity of the resulting \code{data.frame}, it is
#' sensible to restrict the fields returned by \code{\link{ala_occurrences}()}.
#' This function allows easy selection of fields, or commonly-requested groups 
#' of columns, following syntax shared with \code{dplyr::select()}.
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
#' returned by \code{galah_select}. It is only used by \code{ala_counts}, meaning
#' that setting \code{expand = TRUE} will not affect calls to \code{ala_occurrences}.
#' @note \code{select_columns} and \code{galah_select} are synonymous;
#' \code{select_columns} is deprecated and will be removed from future versions
#' of \code{galah}.
#' @seealso \code{\link{select_taxa}}, \code{\link{galah_filter}} and
#' \code{\link{galah_locations}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions; \code{\link{ala_counts}}
#' for how to get counts by levels of variables returned by \code{galah_select}.
#' @importFrom dplyr select
#' @export

galah_select <- function(...,
  group = c("basic", "event", "assertions")
){

  # match 'groups' of columns
  if (!missing(group) && !is.null(group)) {
    group <- match.arg(group, several.ok = TRUE)
    group_cols <- data.table::rbindlist(lapply(group, function(x) {
      type <- ifelse(x == "assertions", "assertions", "field")
      data.frame(name = preset_cols(x), type = type,
                 stringsAsFactors = FALSE)
    }))} else {
      group_cols <- NULL
    }
    
  # build a data.frame with a standardised set of names, stored by galah_config()
  load_fields()
  field_names <- galah_config()$valid_fields 
  df <- as.data.frame(
   matrix(data = NA, nrow = 0, ncol = length(field_names),
     dimnames = list(NULL, field_names)))
  
  # make a data.frame listing valid fields and their type
  all_cols <- data.frame(
    name = unique(c(group_cols, colnames(select(df, ...)))))
  all_cols$type <- ifelse(str_detect(x, "[[:lower:]]"), "field", "assertions")
    
  # add S3 class and return
  class(all_cols) <- append(class(all_cols), "ala_fields") 
  all_cols
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