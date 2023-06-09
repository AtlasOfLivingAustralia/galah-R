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
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  group <- check_groups(group, n = length(parsed_dots$data))
  if(is.null(parsed_dots$data_request)){
    parse_select(parsed_dots$data, group)
  }else{
    update_galah_call(parsed_dots$data_request, 
                      select = parse_select(parsed_dots$data, group))
  }
}

#' @rdname galah_select
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @export
select.data_request <- function(.data, ..., group){
  
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  group <- check_groups(group, n = length(parsed_dots$data))
  update_galah_call(.data, 
                    select = parse_select(parsed_dots$data, group))
}

#' Build a data.frame with a standardised set of names
#' @noRd
#' @keywords Internal
parse_select <- function(dot_names, group){
  
  if(length(group) > 0){
    group_cols <- lapply(group, preset_cols) |> unlist()
  }else{
    group_cols <- NULL
  }
  
  if(length(dot_names) > 0){
    individual_cols <- dot_names 
  }else{ # i.e. no fields selected
    # code an exception here:
    ## because assertions aren't fields, leaving `fields` empty means default fields are returned
    ## but only when `group = assertions` and no other requests are made
    ## this adds a single field (recordID) to the query to avoid this problem
    if(length(group) == 1 && all(group == "assertions")){
      individual_cols <- "recordID"
    }else{
      individual_cols <- NULL 
    }
  }
  
  # create output object
  result <- tibble(name = unique(c(group_cols, individual_cols)))
  result$type <- "field"
  # result$type[result$name %in% show_all("assertions")$id] <- "assertion" # requires upgrade to show_all
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
