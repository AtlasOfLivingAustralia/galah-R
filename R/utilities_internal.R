##---------------------------------------------------------------
##                 Output formatting functions                 --
##---------------------------------------------------------------

#' Internal function to enforce `select()` for metadata queries. Basically just 
#' supplies defaults. This is the *setup* phase as is usually called by 
#' `as_query()`
#' @noRd
#' @keywords Internal
enforce_select_query <- function(new_query, supplied_query){
  # if `select()` is given, we simply pass it on
  # if missing, we have to apply some logic
  if(is.null(supplied_query$select)){
    specific_type <- supplied_query |>
      purrr::pluck("type") |>
      stringr::str_remove("^metadata/")
    # see whether `lookup_select_columns()` returns anything
    chosen_columns <- lookup_select_columns(specific_type)  
    # some `unnest` queries internally rename the lead column to the name of the supplied field
    if(is.null(chosen_columns) & 
       stringr::str_detect(specific_type, "-unnest$")){
      chosen_columns <- supplied_query$filter |>
        purrr::pluck("value")
    }
    # if we have, after 2 attempts, found some chosen_columns, use them
    if(!is.null(chosen_columns)){
      supplied_query <- dplyr::select(supplied_query, 
                                      tidyselect::any_of({{chosen_columns}}))
      # if *still* null, choose `everything()`
    }else{
      supplied_query <- dplyr::select(supplied_query, 
                                      tidyselect::everything()) 
    }
  }
  update_request_object(new_query,
                        select = supplied_query$select)
}

#' Internal function to run `eval_tidy()` on captured `select()` requests.
#' This is the *enactment* phase and is usually called by `collect()`.
#' Critically, this function is *NOT* called by `select()`. This matters because
#' we have to eval `unnest()` before `select()` for it to work, and this can
#' only happen at the end of a pipe.
#' @noRd
#' @keywords Internal
parse_select <- function(df, .query){
  # get quosures captured by `select()`
  quo_list <- purrr::pluck(.query, "select", "quosure")
  # map() over list of quosures
  # honestly I don't know why `!!quo_list` fails here, but it does, so used this instead
  pos <- purrr::map(quo_list, \(a){
    tidyselect::eval_select(expr = a, data = df)
  }) |>
    unlist()
  # apply tidy selection to `df`
  # note: this code taken from `tidyselect` documentation; it could be argued that `df[pos]` is sufficient
  rlang::set_names(df[pos], names(pos)) 
}

#' Internal function to rename specific columns. Note this is safer than calling
#' `dplyr::rename()` directly, because it only seeks to rename columns that 
#' are actually present, and so won't fail.
#' @noRd
#' @keywords Internal
parse_rename <- function(df, .query){
  cols <- colnames(df)
  rename_vec <- .query$type |>
    stringr::str_remove("^metadata/") |>
    lookup_rename_columns()
  # check whether renaming information is given
  if(!is.null(rename_vec)){
    # check whether these are actually present in the supplied `tibble`
    col_lookup <- rename_vec %in% cols
    # if they are, rename
    if(any(col_lookup)){
      rename_cols <- as.list(rename_vec[col_lookup])
      dplyr::rename(df, !!!rename_cols)
    # otherwise, return source `tibble`
    }else{
      df
    }
  # if no lookup information supplied, return source `tibble`
  }else{
    df
  }
}

#' Simple internal function to `arrange()` by first column
#' @noRd
#' @keywords Internal
parse_arrange <- function(df){
  dplyr::arrange(df, dplyr::pull(df, 1))
}

#' Choose column names to pass to `select()`. 
#' NOTE: this isn't especially subtle wrt different atlases
#' NOTE: this assumes `dplyr::rename_with(camel_to_snake_case)` has been run
#' @noRd
#' @keywords Internal
lookup_select_columns <- function(type) {
    switch(type,
           "assertions" = c("id",
                            "description",
                            "category",
                            "type"),
           "fields" = c("id",
                        "description",
                        "type"),
           "identifiers" = lookup_select_columns_taxa(),
           "licences" = c("id",
                          "name",
                          "acronym",
                          "url"),
           "lists" = c("species_list_uid",
                       "list_name",
                       "description",
                       "list_type",
                       "item_count"),
           "lists-unnest" = c("scientific_name",
                              "vernacular_name",
                              "taxon_concept_id"),
           "media" = c("media_id",
                       "occurrence_id",
                       "creator", 
                       "license",
                       "data_resource_uid",
                       "date_taken",
                       "date_uploaded",
                       "mime_type",
                       "size_in_bytes",
                       "success"),
           "profiles" = c("id",
                         "short_name",
                         "name",
                         "description"),
           "profiles-unnest" = c("id",
                                 "description",
                                 "filter",
                                 "enabled"),
           "reasons" = c("id",
                         "name"),
           "taxa" = lookup_select_columns_taxa(),
           "taxa-unnest" = c("name",
                             "taxon_concept_id",
                             "parent_taxon_concept_id",
                             "rank"),
           NULL # When no defaults are set, sending NULL tells the code to call `everything()`
           )
}

#' `lookup_select_columns()` but for taxa and identifier queries
#' @noRd
#' @keywords Internal
lookup_select_columns_taxa <- function(){
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
    # taxonomic ranks (basic only)
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "species"
    # if all are needed, use this instead
    # {show_all_ranks() |> dplyr::pull("name")}
  )
}

#' Choose which columns to rename
#' @noRd
#' @keywords Internal
lookup_rename_columns <- function(type){
  switch(type, 
         "assertions" = c("id" = "name"),
         "identifiers" = c("taxonConceptID" = "key"),
         "lists" = c("species_list_uid" = "data_resource_uid"),
         "lists-unnest" = c("taxon_concept_id" = "lsid"),
         "media" = c("media_id" = "image_identifier"),
         "taxa" = c("class" = "classs",
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
                    "vernacular_name" = "french_vernacular_name"),
         "taxa-unnest" = c("taxon_concept_id" = "guid",
                           "parent_taxon_concept_id" = "parent_guid"),
         NULL
  )
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
#' Primarily for reversing the action of `gbif_upper_case()` (above)
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

##----------------------------------------------------------------
##  Functions to add information to occurrence queries          --
##----------------------------------------------------------------
## Note these now follow `tidyverse` convention of accepting and
## returning same object type

#' Add a logical flag re: whether user should receive an email
#' @param x a list
#' @noRd
#' @keywords Internal
add_email_notify <- function(x) {
  notify <- as.logical(potions::pour("package", "send_email"))
  if(is.na(notify)) {
    notify <- FALSE
  }
  # ala api requires lowercase
  x$email_notify <- ifelse(notify, "true", "false")
  x
}

#' Add an email address, but *only* when JWT tokens are not given
#' @noRd
#' @keywords Internal
add_email_address <- function(x, query){
  if(is.null(query$authenticate)){
    x$email <- potions::pour("user", "email")
  }
  x
}

#' Add a DOI request
#' @noRd
#' @keywords Internal
add_doi_request <- function(x, query){
  if(isTRUE(query$mint_doi) & 
     potions::pour("atlas", "region") == "Australia"){
    x$mintDoi <- TRUE 
  }
  x
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

#' Internal function for determining if we should call ALA or not
#' @noRd
#' @keywords Internal
is_ala <- function(){
  potions::pour("atlas", "region") == "Australia"
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
  supported_atlases <- request_metadata(type = "apis") |>
    collect() |>
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