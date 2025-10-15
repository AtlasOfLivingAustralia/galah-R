##---------------------------------------------------------------
##                 Output formatting functions                 --
##---------------------------------------------------------------

#' Choose column names to pass to `select()`. 
#' NOTE: this isn't especially subtle wrt different atlases
#' NOTE: this assumes `dplyr::rename_with(camel_to_snake_case)` has been run
#' @noRd
#' @keywords Internal
wanted_columns <- function(type) {
    switch(type,
           "assertions" = c("id",
                            "description",
                            "category",
                            "type"),
           "fields" = c("id",
                        "description",
                        "type"),
           "identifiers" = wanted_columns_taxa(),
           "licences" = c("id",
                          "name",
                          "acronym",
                          "url"),
           "lists" = c("species_list_uid",
                       "list_name",
                       "description",
                       "list_type",
                       "item_count"),
           "media" = c("image_id",
                       "creator", 
                       "license",
                       "data_resource_uid",
                       "date_taken",
                       "date_uploaded",
                       "mime_type",
                       "mimetype",
                       "width",
                       "height",
                       "size_in_bytes",
                       "image_url"),
           "profiles" = c("id",
                         "short_name",
                         "name",
                         "description"),
           "reasons" = c("id",
                         "name"),
           "taxa" = wanted_columns_taxa(),
           NULL # When no defaults are set, sending NULL tells the code to call `everything()`
           )
}

#' `wanted_columns()` but for taxa *and* identifier queries
#' @noRd
#' @keywords Internal
wanted_columns_taxa <- function(){
  c("search_term",
    "scientific_name",
    "scientific_name_authorship", 
    "taxon_concept_id", # ALA
    "taxon_concept_lsid", # Austria, Guatemala
    "authority", # OpenObs
    "usage_key", # GBIF
    "guid", # species search
    "canonical_name", "status", 
    "rank",
    "match_type",
    "confidence",
    "time_taken",
    "vernacular_name",
    "issues",
    {show_all_ranks() |> dplyr::pull("name")})
}

#' Internal function to run `eval_tidy()` on captured `select()` requests
#' @noRd
#' @keywords Internal
parse_select <- function(df, .query){
  select_list <- .query |>
    purrr::pluck("select") |>
    purrr::map(rlang::is_quosure) |>
    unlist() |>
    which()
  select_query <- .query |>
    purrr::pluck("select", !!!select_list) 
  pos <- tidyselect::eval_select(expr = select_query,
                                 data = df)
  rlang::set_names(df[pos], names(pos)) # note: this line taken from 
  # `tidyselect` documentation; it could be argued that `df[pos]` is sufficient
}
  
#' Internal function to rename specific columns
#' In-progress, tidyverse-compliant replacement for `rename_columns()`
#' Note that actual renaming is now handled in-pipe by `dplyr::rename()`
#' @noRd
#' @keywords Internal
parse_rename <- function(df, type){
  if(type == "taxa"){
    taxa_vec <- c("class" = "classs",
                  "taxon_concept_id" = "usage_key",
                  "taxon_concept_id" = "guid",
                  "taxon_concept_id" = "reference_id",
                  "taxon_concept_id" = "key",
                  "genus" = "genus_name",
                  "family" = "family_name",
                  "order" = "order_name",
                  "phylum" = "phylum_name",
                  "kingdom" = "kingdom_name",
                  "rank" = "rank_name",
                  "vernacular_name" = "french_vernacular_name")
    cols <- colnames(df)
    col_lookup <- taxa_vec %in% cols
    rename_cols <- as.list(taxa_vec[col_lookup])
    if(any(col_lookup)){
      dplyr::rename(df, !!!rename_cols)
    }else{
      df
    }
  }else if(type == "assertions"){
    dplyr::rename(df, !!!c("id" = "name"))
  }else if(type == "media"){
    dplyr::rename(df, !!!c("image_id" = "image_identifier",
                           "mimetype" = "mime_type"))
  }else{
    df
  }
}


##---------------------------------------------------------------
##                          Cases                              --
##---------------------------------------------------------------

#' Internal function to make text to snake case
#' @noRd
#' @keywords Internal
camel_to_snake_case <- function(string){
  string |>
    gsub("([a-z])([A-Z])", "\\1_\\L\\2", x = _, perl = TRUE) |>
    trimws(which = "both") |> # end spaces
    gsub("\\.+|\\s+", "_", x = _) |> # internal dots or spaces
    tolower()
}

#' Internal function to handle conversion from camelCase to upper snake case
#' @noRd
#' @keywords internal
gbif_upper_case <- function(string){
  gsub("(?=[[:upper:]])", "_", string, perl = TRUE) |> 
    toupper()
}

#' Internal function to handle conversion from upper snake case to camelCase
#' Primarily for reversing the action of [gbif_upper_case()] above
#' @noRd
#' @keywords internal
snake_to_camel_case <- function(string){
  # first split into words
  split_string <- string |>
    tolower() |>
    strsplit("_") |>
    purrr::pluck(!!!list(1))
    
  # then amend only multi-word strings
  word_count <- length(split_string)
  if(word_count > 1){
    c(split_string[1],
      stringr::str_to_title(split_string[seq(2, word_count)])) |>
      glue::glue_collapse()
  }else{
    split_string
  }
}

##---------------------------------------------------------------
##                   Set API header arguments                  --
##---------------------------------------------------------------

# Construct the user agent string, consisting of the galah version
# This is added on to all requests to enable usage monitoring 
galah_version_string <- function() {
  version_string <- "version unknown"
  suppressWarnings(
    try(version_string <- utils::packageDescription("galah")[["Version"]],
        silent = TRUE)) ## get the galah version, if we can
  glue::glue("galah-R {version_string}")
}

#' @noRd
#' @keywords Internal
source_type_id_lookup <- function(region){
  switch(region,
         "Austria" = 1,
         "United Kingdom" = 2001,
         "2004") # ALA default for galah
}

#' @noRd
#' @keywords Internal
email_notify <- function() {
  notify <- as.logical(potions::pour("package", "send_email"))
  if (is.na(notify)) {
    notify <- FALSE
  }
  # ala api requires lowercase
  ifelse(notify, "true", "false")
}

##----------------------------------------------------------------
##  Functions to change behaviour depending on selected `atlas` --
##----------------------------------------------------------------

#' Internal function for determining if we should call GBIF or not
#' @noRd
#' @keywords Internal
is_gbif <- function(){
  potions::pour("atlas", "region") == "Global"
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
  atlas <- potions::pour("atlas", "region")
  if(atlas %in% c("Austria", 
                  "Brazil", 
                  "Guatemala", 
                  "Kew",
                  "Portugal",
                  "United Kingdom")){
    c("id",
      "taxon_name",
      "taxon_concept_lsid",
      "latitude",
      "longitude",
      "occurrence_date",
      "basis_of_record",
      "occurrence_status",
      "data_resource_uid")
  }else if(atlas %in% c("France")){
    c("id", # only difference from ALA
      "scientificName",
      "taxonConceptID",
      "decimalLatitude",
      "decimalLongitude",
      "eventDate",
      "basisOfRecord",
      "occurrenceStatus",
      "dataResourceName")
  }else if(atlas %in% c("Australia",
                        "Flanders",
                        "Spain",
                        "Sweden")){
    c("recordID", # note this requires that the ALA name (`id`) be corrected
      "scientificName",
      "taxonConceptID",
      "decimalLatitude",
      "decimalLongitude",
      "eventDate",
      "basisOfRecord",
      "occurrenceStatus",
      "dataResourceName")
  }else{
    cli::cli_abort("Unknown `atlas`")
  }
}

#' @noRd
#' @keywords Internal
image_fields <- function() {
  atlas <- potions::pour("atlas", "region")
  if(atlas %in% c("Austria", 
                  "Brazil", 
                  "Guatemala", 
                  "Kew",
                  "Portugal",
                  "United Kingdom")){
    "all_image_url"
  }else if(atlas %in% c("Australia",
                        "Flanders",
                        "Spain",
                        "Sweden")){
    c("multimedia", "images", "sounds", "videos")
  }else{
    cli::cli_abort("Unknown `atlas`")
  }
}

#' @noRd
#' @keywords Internal
species_facets <- function(){
  atlas <- potions::pour("atlas", "region")
  if(atlas %in% c("Australia",
                  "Flanders",
                  "France",
                  "Spain",
                  "Sweden")) {
    "speciesID"
  }else{
    "species_guid"
  }
}

#' @noRd
#' @keywords Internal
profiles_supported <- function(){
  atlas <- potions::pour("atlas", "region")
  if(atlas %in% c("Australia",
                  "Flanders",
                  "Sweden",
                  "Spain")) {
    TRUE
  }else{
    FALSE
  }
}

#' Internal function for determining whether a Living Atlas supports reasons API.
#' This affects whether a reason is appended to a query in `collapse()` (and 
#' checked in `compute()`)
#' @noRd
#' @keywords Internal
reasons_supported <- function(){
  atlas <- potions::pour("atlas", "region")
  supported_atlases <- show_all(apis) |>
    dplyr::filter(type == "metadata/reasons") |>
    dplyr::pull(atlas)
  atlas %in% supported_atlases
}

#' @noRd
#' @keywords Internal
media_supported <- function(){
  atlas <- potions::pour("atlas", "region",
                         .pkg = "galah")
  unsupported_atlases <- c("France", "Global")
  if(atlas %in% unsupported_atlases){
    cli::cli_abort("`atlas_media` is not supported for atlas = {atlas}")
  }
}