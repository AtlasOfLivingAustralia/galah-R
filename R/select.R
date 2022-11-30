#' Specify fields for occurrence download
#'
#' The living atlases store content in hundreds of different fields, and users
#' often require thousands or millions of records at a time. To reduce time taken
#' to download data, and limit complexity of the resulting `data.frame`, it is
#' sensible to restrict the fields returned by [atlas_occurrences()].
#' This function allows easy selection of fields, or commonly-requested groups 
#' of columns, following syntax shared with `dplyr::select()`.
#' 
#' The full list of available fields can be viewed with `show_all(fields)`.
#' `r lifecycle::badge("experimental")` 
#' @param ... zero or more individual column names to include
#' @param group `string`: (optional) name of one or more column groups to
#' include. Valid options are `"basic"`, `"event"` and
#' `"assertions"`
#' @return A tibble
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
#'   * `occurrenceStatus`
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
#' Using `group = "media"` returns the following columns:
#' 
#'   * `multimedia`
#'   * `multimediaLicence`
#'   * `images`
#'   * `videos`
#'   * `sounds`
#' 
#' Using `group = "assertions"` returns all quality assertion-related
#' columns. The list of assertions is shown by `show_all_assertions()`.
#' @seealso  [galah_select()], with which this function is synonymous.
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @export
select.data_request <- function(.data, ..., group = c("basic", "event", "media", "assertions")
){
  dots <- enquos(..., .ignore_empty = "all")
  
  # If no args are supplied, set default columns returned as group = "basic"  
  if(missing(group)){
    if(length(dots) < 1){
      group_chosen <- "basic"
    }else{
      group_chosen <- NULL
    }
  }else{
    group_chosen <- match.arg(group, several.ok = TRUE)
  }
  
  update_galah_call(.data, select = parse_select(dots, group_chosen))
}