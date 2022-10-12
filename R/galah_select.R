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
#'
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
#'
#' @seealso [search_taxa()], [galah_filter()] and
#' [galah_geolocate()] for other ways to restrict the information returned
#' by [atlas_occurrences()] and related functions; [atlas_counts()]
#' for how to get counts by levels of variables returned by `galah_select`;
#' `show_all(fields)` to list available fields.
#' 
#' @section Examples: 
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Download occurrence records of *Perameles* taken in 2001, only returning 
#' scientific name and event date
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(email = "your-email@email.com")
#' galah_call() |>
#'   galah_identify("perameles")|>
#'   galah_filter(year == 2001) |>
#'   galah_select(scientificName, eventDate) |>
#'   atlas_occurrences()
#' ```
#' 
#' Download occurrence record of *Perameles* taken in 2001, returning the 
#' "basic" group of columns plus the Basis of Record
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_call() |>
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2001) |>
#'   galah_select(group = c("basic", "event"), basisOfRecord) |>
#'   atlas_occurrences()
#' ```
#' 
#' @importFrom tidyselect eval_select
#' @importFrom rlang as_label
#' @importFrom tibble as_tibble
#' @export
galah_select <- function(...,
                         group = c("basic", "event", "media", "assertions")
                         ) {  

  dots <- enquos(..., .ignore_empty = "all")
  
  # Check to see if any of the inputs are a data request
  if(length(dots) > 0){
    checked_dots <- detect_data_request(dots)
    if(!inherits(checked_dots, "quosures")){
      is_data_request <- TRUE
      data_request <- checked_dots[[1]]
      dots <- checked_dots[[2]]
    }else{
      is_data_request <- FALSE
    }
  }else{
    is_data_request <- FALSE
  }
  
  # If no args are supplied, set default columns returned as group = "basic"  
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
  field_names <- unique(c(show_all_fields()$id, show_all_assertions()$id))
  df <- as.data.frame(
   matrix(data = NA, nrow = 0, ncol = length(field_names),
     dimnames = list(NULL, field_names)))
  
  ## Make a data.frame listing valid fields and their type
  selection <- unlist(lapply(dots, function(a){
    names(tidyselect::eval_select(a, data = df))
    }))
  all_cols <- data.frame(
    name = unique(c(group_cols, selection)))
  all_cols$type <- ifelse(str_detect(all_cols$name, "[[:lower:]]"), "field", "assertions")
    
  # Add S3 class
  all_cols <- as_tibble(all_cols)
  attr(all_cols, "call") <- "galah_select" 
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, select = all_cols)
  }else{
    all_cols
  }
}

# NOTE: gbif doesn't appear to support column specification in downloads

preset_cols <- function(type) {
  cols <- switch(type,
                 "basic" = default_columns(),
                 "event" = c("eventRemarks", "eventTime", "eventID",
                             "eventDate", "samplingEffort",
                             "samplingProtocol"),
                 "media" = c("multimedia", "multimediaLicence", 
                             "images", "videos", "sounds"),
                 "assertions" = show_all_assertions()$id
               )
  return(cols)
}