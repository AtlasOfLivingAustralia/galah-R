#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use [galah_select()] instead of `select_columns()`.
#' 
#' @export
#' @name deprecated
select_columns <- function(..., group){
  lifecycle::deprecate_warn("2.0.0", "select_columns()", "galah_select()")
  
  galah_select(..., group = group)
}

#' @description
#' Use [galah_filter()] instead of `select_filters()`.
#'
#' @export
#' @name deprecated
select_filters <- function(..., profile = NULL) {
  lifecycle::deprecate_warn("2.0.0", "select_filters()", "galah_filter()")
  
  galah_filter(..., profile = profile)
}


#' @description 
#' Use [galah_location()] instead of `select_locations()`.
#'
#' @export
#' @name deprecated
select_locations <- function(query) {
  lifecycle::deprecate_warn("2.0.0", "select_locations()", "galah_geolocate()")
  
  galah_geolocate(query)
}


#' @description 
#' Use [atlas_occurrences()] instead of `ala_occurrences()`.
#'
#' @export
#' @name deprecated
ala_occurrences <- function(taxa = NULL, filters = NULL, locations = NULL,
                            columns = select_columns(group = "basic"),
                            mint_doi = FALSE, doi, refresh_cache = FALSE) {
  lifecycle::deprecate_warn("2.0.0", "ala_occurrences()", "atlas_occurrences()")
  
  atlas_occurrences(
    taxa = taxa, 
    filter = filters, 
    geolocate = locations,
    select = columns,
    mint_doi = mint_doi, 
    doi = doi, 
    refresh_cache = refresh_cache) 
}


#' @description
#' Use [atlas_counts()] instead of `ala_counts()`.
#'
#' @export
#' @name deprecated
ala_counts <- function(taxa = NULL, 
                       filters = NULL, 
                       locations = NULL,
                       limit = 100,
                       type = c("record" ,"species"),
                       refresh_cache = FALSE) {
  type <- match.arg(type)
  
  lifecycle::deprecate_warn("2.0.0", "ala_counts()", "atlas_counts()")
  
  atlas_counts.default(
    taxa = {if(missing(taxa)){NULL}else{taxa}}, 
    filter = {if(missing(filters)){NULL}else{filters}}, 
    geolocate = {if(missing(locations)){NULL}else{locations}},
    limit = limit,
    type = type,
    refresh_cache = refresh_cache)
}


#' @description 
#' Use [atlas_species()] instead of `ala_species()`.
#' 
#' @export
#' @name deprecated
ala_species <- function(taxa = NULL, filters = NULL, locations = NULL,
                        refresh_cache = FALSE) {
  lifecycle::deprecate_warn("2.0.0", "ala_species()", "atlas_species()")
  
  atlas_species(
    taxa = taxa,
    filter = filters,
    geolocate = locations,
    refresh_cache = refresh_cache
  )
}

#' @description
#' Use [atlas_taxonomy()] instead of `ala_taxonomy()`.
#' 
#' @export
#' @name deprecated
ala_taxonomy <- function(taxa, down_to){
  lifecycle::deprecate_warn("2.0.0", "ala_taxonomy()", "atlas_taxonomy()")
  
  atlas_taxonomy(
    taxa = taxa,
    down_to = down_to
  )
}

#' @description
#' Use [atlas_media()] instead of `ala_media()`.
#' 
#' @export
#' @name deprecated
ala_media <- function(taxa = NULL, 
                      filters = NULL, 
                      locations = NULL,
                      columns = select_columns(group = "basic"),
                      download_dir,
                      refresh_cache = FALSE) {
  lifecycle::deprecate_warn("2.0.0", "ala_media()", "atlas_media()")
  
  atlas_media(
    taxa = taxa,
    filter = filters,
    geolocate = geolocate,
    select = columns,
    download_dir = download_dir,
    refresh_cache = FALSE
  )
}

#' @description 
#' Use [atlas_citation()] instead of `ala_citation()`.
#' 
#' @export
#' @name deprecated
ala_citation <- function(data) {
  lifecycle::deprecate_warn("2.0.0", "ala_citation()", "atlas_citation()")
  
  atlas_citation(
    data = data
  )
}
