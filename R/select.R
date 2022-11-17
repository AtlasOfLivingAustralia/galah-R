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
#' @importFrom dplyr select
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @exportS3Method dplyr::select
#' @export
select.data_request <- function(.data, ..., group = c("basic", "event", "media", "assertions")
){
  dots <- enquos(..., .ignore_empty = "all")
  
  # If no args are supplied, set default columns returned as group = "basic"  
  if(missing(group) & length(dots) < 1){
    group <- "basic"
  }
  
  # Build a data.frame with a standardised set of names,
  # stored by galah_config()
  field_names <- unique(c(show_all_fields()$id, show_all_assertions()$id))
  df <- matrix(data = NA, nrow = 0, ncol = length(field_names),
               dimnames = list(NULL, field_names)) |>
    as.data.frame()
  
  # Match 'groups' of columns
  if (!missing(group) && !is.null(group)) {
    append_groups <- TRUE
    group <- match.arg(group, several.ok = TRUE)
    group_cols <- unlist(lapply(group, preset_cols))
    select_groups <- eval_select(all_of(group_cols), data = df) |> names()
  } else {
    append_groups <- FALSE
    select_groups <- NULL
  }
  
  # select
  if(length(dots) > 0){
    select_individuals <- unlist(lapply(dots, function(a){
      tidyselect::eval_select(a, data = df) |> names()
    }))
  }else{
    select_individuals <- NULL
  }
  
  all_cols <- tibble(name = unique(c(select_groups, select_individuals)))
  all_cols$type <- ifelse(str_detect(all_cols$name, "[[:lower:]]"), 
                          "field", 
                          "assertions")
  attr(all_cols, "call") <- "galah_select" 
  if(append_groups){
    attr(all_cols, "groups") <- group 
  }
  
  update_galah_call(.data, select = all_cols)
}