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
#' @importFrom tibble as_tibble
#' @export
galah_select <- function(...,
                         group){  
  dots <- enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           parsed_dots <- parse_quosures_basic(dots[-1])
           group <- check_groups(group, n = length(parsed_dots))
           result <- parse_select(parsed_dots, group)
           update_data_request(dots[[1]], select = result)
         },
         {
           parsed_dots <- parse_quosures_basic(dots)
           group <- check_groups(group, n = length(parsed_dots))
           parse_select(parsed_dots, group)
         })
}

#' @rdname galah_select
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @export
select.data_request <- function(.data, ..., group){
  parsed_dots <- enquos(..., .ignore_empty = "all") |>
    parse_quosures_basic()
  group <- check_groups(group, n = length(parsed_dots))
  update_data_request(.data, select = parse_select(parsed_dots, group))
}

#' Build a data.frame with a standardised set of names
#' @importFrom rlang inform
#' @noRd
#' @keywords Internal
parse_select <- function(dot_names, group){
  if(is_gbif()){
    inform(c("skipping `select()`:",
             i = "This function is not supported by the GBIF API v1"))
    return(NULL)
  }else{
    if(length(group) > 0){
      group_cols <- lapply(group, preset_groups) |> unlist()
    }else{
      group_cols <- NULL
    }
    # set behaviour depending on what names are given
    # NOTE:
    ## because assertions aren't fields, leaving `fields` empty means default fields are returned
    ## but only when `group = assertions` and no other requests are made
    ## this adds a single field (recordID) to the query to avoid this problem.
    ## This problem also occurs when a single field is requested
    ## under some circumstances (e.g. "images"), even when that field is 
    ## fully populated.
    if(length(dot_names) > 1){
      individual_cols <- dot_names 
    }else{ # i.e. no fields selected
      if(length(dot_names) == 1){
        if(length(group) == 0){
          individual_cols <- unique(c("recordID", dot_names))
        }else{
          individual_cols <- dot_names
        }
      }else{ # i.e. length(dot_names) == 0
        if(length(group) == 1 & !any(group == "basic")){
          individual_cols <- "recordID"
        }else{
          individual_cols <- NULL
        }
      }
    }
    # create output object
    # NOTE: placing `recordID` first is critical;
    # having e.g. media columns _before_ `recordID` causes the download to fail 
    values <- unique(c(group_cols, individual_cols))
    values <- c("recordID", values[values != "recordID"])
    result <- tibble(name = values)
    result$type <- ifelse(!grepl("[[:lower:]]", values), "assertion", "field")
    # result$type[result$name %in% show_all("assertions")$id] <- "assertion" 
    ## above line commented out as it breaks our rule about pinging an API before
    ## `compute()` is called. Instead currently uses all-upper-case check
    attr(result, "group") <- group
    return(result) 
  }
}

#' Internal function to populate `groups` arg in `select()`
#' @noRd
#' @keywords Internal
preset_groups <- function(group_name) {
  cols <- switch(group_name,
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

#' Internal function to specify 'basic' columns in `select()`
#' @noRd
#' @keywords Internal
default_columns <- function() {
  atlas <- pour("atlas", "region")
  switch (atlas,
          "Guatemala" = c("latitude", "longitude", "species_guid",
                          "data_resource_uid", "occurrence_date", "id"),
          "Spain" = c("latitude", "longitude", "species_guid",
                      "data_resource_uid", "occurrence_date", "recordID"),
          c("decimalLatitude", "decimalLongitude", "eventDate",
            "scientificName", "taxonConceptID",
            "recordID", # note this requires that the ALA name (`id`) be corrected
            "dataResourceName",
            "occurrenceStatus")
  )
}