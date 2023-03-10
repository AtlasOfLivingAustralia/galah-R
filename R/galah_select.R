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
#' @examples \dontrun{
#' # Download occurrence records of *Perameles*, 
#' # Only return scientificName and eventDate columns
#' galah_config(email = "your-email@email.com")
#' galah_call() |>
#'   galah_identify("perameles")|>
#'   galah_select(scientificName, eventDate) |>
#'   atlas_occurrences()
#' 
#' # Only return the "basic" group of columns and the basisOfRecord column
#' galah_call() |>
#'   galah_identify("perameles") |>
#'   galah_select(basisOfRecord, group = "basic") |>
#'   atlas_occurrences()
#' }
#' 
#' @importFrom tidyselect eval_select
#' @importFrom tidyselect all_of
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
  
  if(getOption("galah_config")$atlas$region == "Global"){
    message("GBIF does not support `select` queries")
    if(is_data_request){
      return(data_request)
    }else{
      return(NULL)
    }
  }
  
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
  
  result <- parse_select(dots, group_chosen)
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, select = result)
  }else{
    result
  }
}


# Build a data.frame with a standardised set of names
parse_select <- function(dots, group){
  current_assertions <- show_all_assertions()
  field_names <- unique(c(show_all_fields()$id, current_assertions$id))
  df <- matrix(data = NA, nrow = 0, ncol = length(field_names),
               dimnames = list(NULL, field_names)) |>
    as.data.frame()
  
  if(length(group) > 0){
    group_cols <- lapply(group, preset_cols) |>
                  unlist()
    select_groups <- eval_select(all_of(group_cols), data = df) |> 
                     names()
  }else{
    select_groups <- NULL
    group <- ""
  }
  
  if(length(dots) > 0){
    select_individuals <- unlist(lapply(dots, function(a){
      eval_select(a, data = df) |> 
      names()
    }))
  }else{ # i.e. no fields selected
    # code an exception here:
    ## because assertions aren't fields, leaving `fields` empty means default fields are returned
    ## but only when `group = assertions` and no other requests are made
    ## this adds a single field (recordID) to the query to avoid this problem
    if(length(group) == 1 && all(group == "assertions")){
      select_individuals <- "recordID"
    }else{
      select_individuals <- NULL 
    }
  }
  
  # create output object
  result <- tibble(name = unique(c(select_groups, select_individuals)))
  result$type <- "field"
  result$type[result$name %in% current_assertions$id] <- "assertion"
  attr(result, "call") <- "galah_select" 
  attr(result, "group") <- group
  
  return(result)
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