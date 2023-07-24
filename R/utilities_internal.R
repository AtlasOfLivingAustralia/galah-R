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
           "checklist" = c("kingdom", "phylum", "class", "order", "family",
                           "genus", "species", "author", "species_guid",
                           "vernacular_name"),
           "profile" = c("id", "shortName", "name", "description"),
           "media" = c("media_id",
                       "creator", "license",
                       "data_resource_uid",
                       "date_taken", "date_uploaded",
                       "mime_type", "width", "height", "size_in_bytes",
                       "image_url"
                     ),
           "layer" = c("id", "description", "source_link"),
           "fields" = c("id", "description"),
           "assertions" = c("id", "description", "category"),
           "quality_filter" = c("description", "filter"),
           "reasons" = c("id", "name"))
}

# Rename specific columns, and convert to snake_case
rename_columns <- function(varnames, type) {
    if (type == "media") {
        varnames[varnames == "imageIdentifier"] <- "media_id"
    }
    else if (type == "taxa") {
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
    } else if (type == "layer") {
        varnames[varnames == "displayname"] <- "name"
        varnames[varnames == "source_link"] <- "link"
    } else if (type == "fields") {
      varnames[varnames == "name"] <- "id"
      varnames[varnames == "info"] <- "description"
    } else if (type == "assertions") {
      varnames[varnames == "name"] <- "id"
    } else if (type == "checklist") {
      varnames[varnames == "Scientific Name Authorship"] <- "author"
      varnames[varnames == "Species"] <- "species_guid"
      varnames[varnames == "Species Name"] <- "species"
    }
    # change all to snake case?
    if (type %in% c("taxa", "media")) {
        varnames <- tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", varnames,
                         perl = TRUE))
        varnames <- tolower(gsub("\\.", "_", varnames))
    } else if (type == "checklist") {
      varnames <- tolower(gsub("\\.|\\s", "_", varnames))
    } else if (type == "occurrence") {
      # change dots to camel case
      varnames <- gsub("\\.(\\w?)", "\\U\\1", varnames, perl = TRUE)
      # replace first letters with lowercase, but only if it is not an
      # all-uppercase field name (which assertions are)
      not_all_uppercase <- str_detect(varnames, "[[:lower:]]")
      substr(varnames[not_all_uppercase], 1, 1) <-
        tolower(substr(varnames[not_all_uppercase], 1, 1))
    }
    varnames
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

##---------------------------------------------------------------
##                Data request helper functions                --
##---------------------------------------------------------------


## show_all_atlases / search_atlases --------------------------#

image_fields <- function() {
  atlas <- pour("atlas", "region")
  switch (atlas,
          "Austria" = "all_image_url",
          "Guatemala" = "all_image_url",
          "Spain" = "all_image_url",
          c("images", "videos", "sounds")
  )
}

species_facets <- function(){
  atlas <- pour("atlas", "region")
  switch(atlas,
         "Australia" = "speciesID",
         # "Austria" = "species_guid",
         # "Brazil" = "species_guid",
         # "Canada" = "species_guid"
         "species_guid"
  )
}

##---------------------------------------------------------------
##                      System down message                    --
##---------------------------------------------------------------

system_down_message <- function(function_name){
  bullets <- c(
    glue("Calling the API failed for `{function_name}`."),
    i = "This might mean that the API is down, or that you are not connected to the internet",
    i = "Double check that your query is correct, or try again later"
  )
  inform(bullets)
}

