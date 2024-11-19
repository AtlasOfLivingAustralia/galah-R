##---------------------------------------------------------------
##                 Output formatting functions                 --
##---------------------------------------------------------------

# Select column names to return
# Subsets data returned by webservices to useful columns
wanted_columns <- function(type) {
    switch(type,
           "taxa" = c("search_term", "scientific_name",
                      "scientific_name_authorship", 
                      "taxon_concept_id", # ALA
                      "taxon_concept_lsid", # Austria, Guatemala
                      "authority", # OpenObs
                      "usage_key", # GBIF
                      "guid", # species search
                      "canonical_name", "status", 
                      "rank",
                      "match_type", "kingdom", "phylum", "class", "order",
                      "family", "genus", "species", "vernacular_name",
                      "issues","subkingdom", "superclass", "infraclass",
                      "subclass", "subinfraclass", "suborder", "superorder",
                      "infraorder", "infrafamily", "superfamily", "subfamily",
                      "subtribe", "subgenus", "subspecies"),
           "extended_taxa" = c("subkingdom", "superclass", "infraclass",
                               "subclass", "subinfraclass", "suborder",
                               "superorder", "infraorder", "infrafamily",
                               "superfamily", "subfamily", "subtribe",
                               "subgenus"),
           "profile" = c("id", "shortName", "name", "description"),
           "media" = c("image_id",
                       "creator", "license",
                       "data_resource_uid",
                       "date_taken", "date_uploaded",
                       "mime_type", "mimetype",
                       "width", "height", "size_in_bytes",
                       "image_url"
                     ),
           "layer" = c("id", "description", "source_link"),
           "fields" = c("id", "description"),
           "assertions" = c("id", "description", "category"),
           "quality_filter" = c("description", "filter"),
           "reasons" = c("id", "name"))
}

#' Internal function to rename specific columns, and convert to snake_case
#' @noRd
#' @keywords Internal
rename_columns <- function(varnames, type) {
  varnames <- camel_to_snake_case(varnames)
  switch(type,
    "media" = {
      varnames[varnames %in% c("image_identifier")] <- "image_id"
      varnames[varnames == "mime_type"] <- "mimetype"
    },
    "taxa" = {
      varnames[varnames == "classs"] <- "class"
      varnames[varnames %in% c("usage_key", "usageKey", "guid", "reference_id", "referenceId")] <- "taxon_concept_id"
      varnames[varnames %in% c("genus_name", "genusName")] <- "genus"
      varnames[varnames %in% c("family_name", "familyName")] <- "family"
      varnames[varnames %in% c("order_name", "orderName")] <- "order"
      varnames[varnames %in% c("class_name", "className")] <- "class"
      varnames[varnames %in% c("phylum_name", "phylumName")] <- "phylum"
      varnames[varnames %in% c("kingdom_name", "kingdomName")] <- "kingdom"
      varnames[varnames %in% c("rank_name", "rankName")] <- "rank"
      varnames[varnames %in% c("french_vernacular_name", "frenchVernacularName")] <- "vernacular_name"
    },
    "assertions" = {
      varnames[varnames == "name"] <- "id"
    },
    "checklist" = {
      varnames[1] <- "taxon_concept_id"
      varnames[varnames %in% c("counts", "number_of_records")] <- "count"
    }
  )
  varnames
}

#' Internal function to make text to snake case
#' @noRd
#' @keywords Internal
camel_to_snake_case <- function(x){
  x |>
    gsub("([a-z])([A-Z])", "\\1_\\L\\2", x = _, perl = TRUE) |>
    trimws(which = "both") |> # end spaces
    gsub("\\.+|\\s+", "_", x = _) |> # internal dots or spaces
    tolower()
}

##---------------------------------------------------------------
##                   Other helpful functions                   --
##---------------------------------------------------------------

# Construct the user agent string, consisting of the galah version
# This is added on to all requests to enable usage monitoring 
galah_version_string <- function() {
  version_string <- "version unknown"
  suppressWarnings(
    try(version_string <- utils::packageDescription("galah")[["Version"]],
        silent = TRUE)) ## get the galah version, if we can
  paste0("galah-R ", version_string)
}

#' Internal function for determining if we should call GBIF or not
#' @importFrom potions pour
#' @noRd
#' @keywords Internal
is_gbif <- function(){
  pour("atlas", "region") == "Global"
}

#' Internal function for determining whether a Living Atlas supports reasons API.
#' This affects whether a reason is appended to a query in `collapse()` (and 
#' checked in `compute()`)
#' @importFrom potions pour
#' @noRd
#' @keywords Internal
atlas_supports_reasons_api <- function(){
  atlas <- pour("atlas", "region")
  supports_reasons <- c("Australia", "Austria", "Guatemala", "Portugal", 
                        "Spain", "Sweden", "United Kingdom")
  atlas %in% supports_reasons
  
  ## List of atlases that support reasons can be checked by running:
  # show_all(apis) |>
  #   dplyr::filter(type == "metadata/reasons") |>
  #   dplyr::pull(atlas)
}

##---------------------------------------------------------------
##                Data request helper functions                --
##---------------------------------------------------------------


## show_all_atlases / search_atlases --------------------------#

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
                 "media" = image_fields(),
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
          "Brazil" = c("id",
                       "taxon_name",
                       "taxon_concept_lsid",
                       "latitude",
                       "longitude",
                       "occurrence_date",
                       "occurrence_status",
                       "data_resource_uid"),
          "France" = c("id",
                       "scientificName",
                       "taxonConceptID",
                       "decimalLatitude",
                       "decimalLongitude",
                       "eventDate",
                       "occurrenceStatus",
                       "dataResourceUid"),
          "Guatemala" = c("id",
                          "taxon_name",
                          "taxon_concept_lsid",
                          "latitude",
                          "longitude",
                          "occurrence_date",
                          "occurrence_status",
                          "data_resource_uid"),
          "Portugal" = c("id",
                         "taxon_name",
                         "taxon_concept_lsid",
                         "latitude",
                         "longitude",
                         "occurrence_date",
                         "occurrence_status",
                         "data_resource_uid"),
          "Spain" = c("recordID",
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

#' @noRd
#' @keywords Internal
image_fields <- function() {
  atlas <- pour("atlas", "region")
  switch (atlas,
          "Austria" = "all_image_url",
          "Australia" = c("multimedia", "images", "sounds", "videos"),
          "Brazil" = "all_image_url",
          "Guatemala" = "all_image_url",
          "Portugal" = "all_image_url",
          "Spain" = c("multimedia", "images", "sounds", "videos"),
          "Sweden" = c("multimedia", "images", "videos", "sounds"),
          "United Kingdom" = "all_image_url"
          # Guatemala ?
  )
}

#' @noRd
#' @keywords Internal
species_facets <- function(){
  atlas <- pour("atlas", "region")
  if(atlas %in% c("Australia", "France", "Spain", "Sweden")) {
    "speciesID"
  }else{
    "species_guid"
  }
}

#' @noRd
#' @keywords Internal
source_type_id_lookup <- function(region){
  switch(region,
         "Austria" = 1,
         "United Kingdom" = 2001,
         "2004") # ALA default for galah
}