#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use [galah_identify()] instead of `select_taxa()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
select_taxa <- function(query, is_id = FALSE) {
  lifecycle::deprecate_warn("1.4.0", "select_taxa()", "galah_identify()")
  if(is_id){
    galah_identify(query, search = FALSE)
  }else{
    result <- galah_identify(query, search = TRUE) |> as.data.frame()
    class(result) <- append(class(result), "galah_identify")
    return(result)
  }
}

#' @description
#' Use [galah_select()] instead of `select_columns()`.
#' 
#' @keywords internal
#' @export
#' @name deprecated
select_columns <- function(..., group){
  lifecycle::deprecate_warn("1.4.0", "select_columns()", "galah_select()")
  
  galah_select(..., group = group)
}

#' @description
#' Use [galah_filter()] instead of `select_filters()`.
#'
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
#'
#' @keywords internal
#' @export
#' @name deprecated
ala_occurrences <- function(taxa = NULL, filters = NULL, locations = NULL,
                            columns = select_columns(group = "basic"),
                            mint_doi = FALSE, doi = NULL, refresh_cache = FALSE) {
  lifecycle::deprecate_warn("1.4.0", "ala_occurrences()", "atlas_occurrences()")
  
  atlas_occurrences_internal(
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
#'
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
  
  atlas_species_internal(
    identify = taxa,
    filter = filters,
    geolocate = locations,
    refresh_cache = refresh_cache
  ) |>
    as.data.frame()
}

#' @description
#' Use [atlas_taxonomy()] instead of `ala_taxonomy()`.
#' 
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
#' 
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
    select = columns,
    download_dir = download_dir,
    refresh_cache = FALSE
  ) |>
    as.data.frame()
}

#' @description 
#' Use [atlas_citation()] instead of `ala_citation()`.
#' 
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
#'
#' @keywords internal 
#' @export
#' @name deprecated
ala_config <- function(..., profile_path = NULL) {
  lifecycle::deprecate_warn("1.3.0", "ala_config()", "galah_config()")

  galah_config(..., profile_path = profile_path)
}
