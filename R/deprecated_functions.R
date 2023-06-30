#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use [galah_identify()] instead of `select_taxa()`.
#' 
#' @keywords internal
#' @export
#' @param query A vector of taxonomic names (for `select_taxa()`) or a valid
#' well-known text string (for `select_locations()`).
#' @param is_id Logical: Does the information provided in `query` consist of 
#' taxonomic identifiers (TRUE), or taxonomic names (FALSE, the default).
#' @name deprecated
#' @return All deprecated functions return a `tibble`.
select_taxa <- function(query, is_id = FALSE) {
  lifecycle::deprecate_warn("1.4.0", "select_taxa()", "galah_identify()")
  if(is_id){
    galah_identify(query, search = FALSE)
  }else{
    result <- galah_identify(query, search = TRUE) |> as.data.frame()
    attr(result, "call") <- "galah_identify"
    return(result)
  }
}

#' @description
#' Use [galah_select()] instead of `select_columns()`.
#' @param ... Queries consisting of field names (for `select_columns()`) or 
#' equations (for `select_filters()`).
#' @param group optional; the name of a group of columns to include. See 
#' `galah_select()` for details.
#' @keywords internal
#' @export
#' @name deprecated
select_columns <- function(..., group){
  lifecycle::deprecate_warn("1.4.0", "select_columns()", "galah_select()")
  
  galah_select(..., group = group)
}

#' @description
#' Use [galah_filter()] instead of `select_filters()`.
#' @param profile String: The name of a valid ALA profile.
#' @keywords internal
#' @export
#' @name deprecated
select_filters <- function(..., profile = NULL) {
  lifecycle::deprecate_warn("1.4.0", "select_filters()", "galah_filter()")
  
  galah_filter(..., profile = profile)
}


#' @description 
#' Use [galah_geolocate()] instead of `select_locations()`.
#'
#' @keywords internal
#' @export
#' @name deprecated
select_locations <- function(query) {
  lifecycle::deprecate_warn("1.4.0", "select_locations()", "galah_geolocate()")
  
  galah_geolocate(query)
}


#' @description 
#' Use [atlas_occurrences()] instead of `ala_occurrences()`.
#' @param taxa A tibble created with `galah_identify()`
#' @param filters A tibble created with `galah_filter()`
#' @param locations A tibble created with `galah_geolocate()`
#' @param columns A tibble created with `galah_select()`
#' @param mint_doi Logical: Should a DOI be created for this download? Defaults
#' to FALSE.
#' @param doi String: Optional ALA DOI to download
#' @param refresh_cache Logical: Should the data be re-downloaded to the cache?
#' Defaults to FALSE.
#' @keywords internal
#' @export
#' @name deprecated
ala_occurrences <- function(taxa = NULL, filters = NULL, locations = NULL,
                            columns = select_columns(group = "basic"),
                            mint_doi = FALSE, doi = NULL, refresh_cache = FALSE) {
  lifecycle::deprecate_warn("1.4.0", "ala_occurrences()", "atlas_occurrences()")
  
  if(is_gbif()){
    abort("Use of `ala_occurrences for GBIF queries is not supported; use `atlas_occurrences` instead")
  }
  
  occurrences_LA(
    identify = taxa, 
    filter = filters, 
    geolocate = locations,
    select = columns,
    mint_doi = mint_doi, 
    doi = doi, 
    refresh_cache = refresh_cache) |> as.data.frame()
}


#' @description
#' Use [atlas_counts()] instead of `ala_counts()`.
#' @param limit Integer: How many records should be downloaded? Defaults to 100.
#' @param type String: Should counts be made of the number of `"records"` 
#' (default) or `"species"`?
#' @keywords internal
#' @export
#' @name deprecated
ala_counts <- function(taxa = NULL, 
                       filters = NULL, 
                       locations = NULL,
                       limit = 100,
                       type = c("record" ,"species"),
                       refresh_cache = FALSE) {
  type <- match.arg(type)
  
  lifecycle::deprecate_warn("1.4.0", "ala_counts()", "atlas_counts()")
  
  atlas_counts_internal(
    identify = {if(missing(taxa)){NULL}else{taxa}}, 
    filter = {if(missing(filters)){NULL}else{filters}}, 
    geolocate = {if(missing(locations)){NULL}else{locations}},
    limit = limit,
    type = type,
    refresh_cache = refresh_cache) |> as.data.frame()
}


#' @description 
#' Use [atlas_species()] instead of `ala_species()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
ala_species <- function(taxa = NULL, filters = NULL, locations = NULL,
                        refresh_cache = FALSE) {
  lifecycle::deprecate_warn("1.4.0", "ala_species()", "atlas_species()")
  
  atlas_species(
    identify = taxa,
    filter = filters,
    geolocate = locations,
    refresh_cache = refresh_cache
  ) |>
    as.data.frame()
}

#' @description
#' Use [atlas_taxonomy()] instead of `ala_taxonomy()`.
#' @param down_to A tibble created with `galah_down_to()`
#' @keywords internal
#' @export
#' @name deprecated
ala_taxonomy <- function(taxa, down_to){
  lifecycle::deprecate_warn("1.4.0", "ala_taxonomy()", "atlas_taxonomy()")
  
  atlas_taxonomy_internal(
    identify = taxa,
    down_to = down_to
  )
}

#' @description
#' Use [atlas_media()] instead of `ala_media()`.
#' @param download_dir Path to a directory in which to place media files
#' @keywords internal
#' @export
#' @name deprecated
ala_media <- function(taxa = NULL, 
                      filters = NULL, 
                      locations = NULL,
                      columns = select_columns(group = "basic"),
                      download_dir,
                      refresh_cache = FALSE) {
  lifecycle::deprecate_warn("1.4.0", "ala_media()", "atlas_media()")
  
  atlas_media_internal(
    identify = taxa,
    filter = filters,
    geolocate = locations,
    download_dir = download_dir,
    refresh_cache = FALSE
  ) |>
    as.data.frame()
}

#' @description 
#' Use [atlas_citation()] instead of `ala_citation()`.
#' @param data A tibble returned by `ala_occurrences()`
#' @keywords internal
#' @export
#' @name deprecated
ala_citation <- function(data) {
  lifecycle::deprecate_warn("1.4.0", "ala_citation()", "atlas_citation()")
  
  atlas_citation(
    data = data
  )
}

#' @description 
#' Use [show_all_reasons()] instead of `find_reasons()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
find_reasons <- function() {
  lifecycle::deprecate_warn("1.4.0", "find_reasons()", "show_all_reasons()")
  
  return(show_all_reasons()) |> as.data.frame()
}

#' @description 
#' Use [show_all_cached_files()] instead of `find_cached_files()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
find_cached_files <- function() {
  lifecycle::deprecate_warn("1.4.0", "find_cached_files()", 
                            "show_all_cached_files()")
  
  show_all_cached_files()
}

#' @description 
#' Use [show_all_ranks()] instead of `find_ranks()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
find_ranks <- function() {
  lifecycle::deprecate_warn("1.4.0", "find_ranks()", "show_all_ranks()")
  
  show_all_ranks() |> as.data.frame()
}

#' @description 
#' Use [show_all_profiles()] instead of `find_profiles()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
find_profiles <- function() {
  lifecycle::deprecate_warn("1.4.0", "find_profiles()", "show_all_profiles()")
  
  show_all_profiles() |> as.data.frame()
}

#' @description 
#' Use [show_all_atlases()] instead of `find_atlases()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
find_atlases <- function() {
  lifecycle::deprecate_warn("1.4.0", "find_atlases()", "show_all_atlases()")
  
  show_all_atlases() |> as.data.frame()
}

#' @description 
#' Use [galah_config()] instead of `ala_config()`.
#' @param profile_path Optional path to profile information
#' @keywords internal 
#' @export
#' @name deprecated
ala_config <- function(..., profile_path = NULL) {
  lifecycle::deprecate_stop("1.3.0", "ala_config()", "galah_config()")

  galah_config(..., profile_path = profile_path)
}


#' @description 
#' Use [show_values()] instead of `search_field_values()`
#' @param field String: A valid field for which to display entries.
#' @keywords internal 
#' @export
#' @name deprecated
search_field_values <- function(field){
  lifecycle::deprecate_warn(when = "1.5.0", 
                            what = "search_field_values()", 
                            details = "Please use `search_fields('query') |> show_values()` instead.")
  search_all("fields", as.character(field)) |> show_values()
}


#' @description 
#' Use [show_values()] instead of `search_profile_attributes()`
#' 
#' @keywords internal 
#' @export
#' @name deprecated
search_profile_attributes <- function(profile){
  lifecycle::deprecate_warn(when = "1.5.0", 
                            what = "search_profile_attributes()", 
                            details = "Please use `search_profiles('query') |> show_values()` instead.")
  search_profiles(paste({profile})) |> show_values()
}
