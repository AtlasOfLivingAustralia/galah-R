#' @title Specify fields for occurrence download
#'
#' @description GBIF nodes store content in hundreds of 
#' different fields, and users often require thousands or millions of records at 
#' a time. To reduce time taken to download data, and limit complexity of the 
#' resulting `tibble`, it is sensible to restrict the fields returned by 
#' [atlas_occurrences()]. This function allows easy selection of fields, or 
#' commonly-requested groups of columns, following syntax shared with 
#' `dplyr::select()`.
#' 
#' The full list of available fields can be viewed with `show_all(fields)`. Note
#' that `select()` and `galah_select()` are supported for all atlases that allow 
#' downloads, with the exception of GBIF, for which all columns are returned.
#'
#' @param ... zero or more individual column names to include
#' @param group `string`: (optional) name of one or more column groups to
#' include. Valid options are `"basic"`, `"event"` `"taxonomy"`, `"media"` and
#' `"assertions"`.
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
#' Using `group = "taxonomy"` returns higher taxonomic information for a given
#' query. It is the only `group` that is accepted by `atlas_species()` as well 
#' as `atlas_occurrences()`.
#' 
#' Using `group = "assertions"` returns all quality assertion-related
#' columns. The list of assertions is shown by `show_all_assertions()`.
#' 
#' For `atlas_occurrences()`, arguments passed to `...` should be valid field
#' names, which you can check using `show_all(fields)`. For `atlas_species()`,
#' it should be one or more of:
#' 
#'   * `counts` to include counts of occurrences per species.
#'   * `synonyms` to include any synonymous names.
#'   * `lists` to include authoritiative lists that each species is included on.
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
#'   
#' # When used in a pipe, `galah_select()` and `select()` are synonymous.
#' # Hence the previous example can be rewritten as:
#' request_data() |>
#'   identify("perameles") |>
#'   select(basisOfRecord, group = "basic") |>
#'   collect()
#' }
#' @importFrom rlang inform
#' @export
galah_select <- function(..., group){
  dots <- enquos(..., .ignore_empty = "all") |>
    detect_request_object() |>
    as.list()
  if(is_gbif()){
    inform("`select()` is not supported for GBIF: skipping")
    if(inherits(dots[[1]], "data_request")){
      dots[[1]]
    }else{
      NULL
    }
  }else{
    dots <- dots |>
      add_summary() |>
      add_group(group)
    if(inherits(dots[[1]], "data_request")){
      update_data_request(dots[[1]], select = dots[-1]) 
    }else{
      dots
    } 
  }
}

#' @rdname galah_select
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @export
select.data_request <- function(.data, ..., group){
  if(is_gbif()){
    inform("`select()` is not supported for GBIF: skipping")
    .data
  }else{
    dots <- enquos(..., .ignore_empty = "all") |>
      as.list() |>
      add_summary() |>
      add_group(group)
    update_data_request(.data, select = dots)  
  }
}

#' internal function to summarise select function (to support `print()`)
#' @importFrom rlang as_label
#' @noRd
#' @keywords Internal
add_summary <- function(dots){
  labels <- lapply(dots, as_label) |>
    unlist() 
  labels <- labels[labels != "<dat_rqst>"]
  last_entry <- length(dots) + 1
  dots[[last_entry]] <- paste(labels, collapse = " | ")
  names(dots)[last_entry] <- "summary"
  dots
}

#' internal function to add `group` arg to the end of a list
#' @noRd
#' @keywords Internal
add_group <- function(dots, group){
  group <- check_groups(group, n = length(dots))
  summary_length <- nchar(dots$summary)
  if(is.null(group)){
    if(summary_length < 1){
      group <- "basic"
      dots$group <- group
    }else{
      dots$group <- vector(mode = "character", length = 0L) 
    }
  }else{
    dots$group <- group
  }
  if(length(dots$group) > 0){
    if(summary_length < 1){
      separator <- ""
    }else{
      separator <- " | "
    }
    dots$summary <- paste0(dots$summary,
                           separator,
                           "group = ", 
                           paste(group, collapse = ", ")) 
  }
  dots
}

#' Internal function to populate `groups` arg in `select()`
#' @noRd
#' @keywords Internal
preset_groups <- function(group_name) {
  cols <- switch(group_name,
                 "basic" = default_columns(),
                 "event" = c("eventRemarks",
                             "eventTime",
                             "eventID",
                             "eventDate",
                             "samplingEffort",
                             "samplingProtocol"),
                 "media" = c("multimedia",
                             "images",
                             "videos",
                             "sounds"),
                 "taxonomy" = c("kingdom",
                                "phylum",
                                "class", 
                                "order", 
                                "family",
                                "genus",
                                "species",
                                "subspecies"))
  # note: assertions handled elsewhere
  return(cols)
}

#' Internal function to specify 'basic' columns in `select()`
#' @noRd
#' @keywords Internal
default_columns <- function() {
  atlas <- pour("atlas", "region")
  switch (atlas,
          "Austria" = c("id",
                        "taxon_name",
                        "taxon_concept_lsid",
                        "latitude",
                        "longitude",
                        "occurrence_date",
                        "occurrence_status",
                        "data_resource_uid"),
          "Guatemala" = c("id",
                          "taxon_name",
                          "taxon_concept_lsid",
                          "latitude",
                          "longitude",
                          "occurrence_date",
                          "occurrence_status",
                          "data_resource_uid"),
          "Spain" = c("id",
                      "scientificName",
                      "taxonConceptID",
                      "decimalLatitude",
                      "decimalLongitude",
                      "eventDate",
                      "occurrenceStatus",
                      "dataResourceUid"),
          "United Kingdom" = c("id",
                               "taxon_name",
                               "taxon_concept_lsid",
                               "latitude",
                               "longitude",
                               "occurrence_date",
                               "occurrence_status",
                               "data_resource_uid"),
          c("recordID", # note this requires that the ALA name (`id`) be corrected
            "scientificName",
            "taxonConceptID",
            "decimalLatitude",
            "decimalLongitude",
            "eventDate",
            "occurrenceStatus",
            "dataResourceName")
  )
}
