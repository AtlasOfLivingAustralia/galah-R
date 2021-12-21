#' Specify fields for occurrence download
#'
#' The living atlases store content on hundreds of different fields, and users
#' often require thousands or millions of records at a time. To reduce time taken
#' to download data, and limit complexity of the resulting `data.frame`, it is
#' sensible to restrict the fields returned by [atlas_occurrences()].
#' This function allows easy selection of fields, or commonly-requested groups 
#' of columns, following syntax shared with `dplyr::select()`.
#' 
#' @param ... zero or more individual column names to include
#' @param group `string`: (optional) name of one or more column groups to
#' include. Valid options are `"basic"`, `"event"` and
#' `"assertion"`
#' @return An object of class `data.frame` and `galah_select`
#' specifying the name and type of each column to include in the 
#' call to `atlas_counts()` or `atlas_occurrences()`.
#' @details
#' Calling the argument `group = "basic"` returns the following columns:
#'
#'   * `decimalLatitude`
#'   * `decimalLongitude`
#'   * `eventDate`
#'   * `scientificName`
#'   * `taxonConceptID`
#'   * `recordID`
#'   * `dataResourceName`
#' 
#' Using `group = "event"` returns the following columns:
#' 
#'   * `eventRemarks`
#'   * `eventTime`
#'   * `eventID`
#'   * `eventDate`
#'   * `samplingEffort`
#'   * `samplingProtocol`
#' 
#' Using `group = "assertions"` returns all quality assertion-related
#' columns. The list of assertions is shown by `search_fields(type = "assertions")`.
#'
#' @seealso [search_taxa()], [galah_filter()] and
#' [galah_geolocate()] for other ways to restrict the information returned
#' by [atlas_occurrences()] and related functions; [atlas_counts()]
#' for how to get counts by levels of variables returned by `galah_select`.
#' @importFrom dplyr select
#' @export
galah_select <- function(...,
                         group = c("basic", "event", "assertions")
                         ) {

  # If no args are supplied, set default columns returned as group = "basic"  
  dots <- as.list(match.call(expand.dots = FALSE)$...)
  if(missing(group) & length(dots) < 1){group <- "basic"}
  
  # Match 'groups' of columns
  if (!missing(group) && !is.null(group)) {
    group <- match.arg(group, several.ok = TRUE)
    group_cols <- unlist(lapply(group, preset_cols))
  } else {
    group_cols <- NULL
  }
    
  # Build a data.frame with a standardised set of names,
  # stored by galah_config()
  load_fields()
  field_names <- show_all_fields()$id 
  df <- as.data.frame(
   matrix(data = NA, nrow = 0, ncol = length(field_names),
     dimnames = list(NULL, field_names)))
  
  # Make a data.frame listing valid fields and their type
  all_cols <- data.frame(
    name = unique(c(group_cols, colnames(select(df, ...)))))
  all_cols$type <- ifelse(str_detect(all_cols$name, "[[:lower:]]"), "field", "assertions")
    
  # Add S3 class
  class(all_cols) <- append(class(all_cols), "galah_select") 
  
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
