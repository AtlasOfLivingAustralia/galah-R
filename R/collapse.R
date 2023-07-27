# if calling `collapse()` after `request_data()`
#' @rdname collect.data_request
#' @export
collapse.data_request <- function(.data){
  .data$type <- check_type(.data$type)
  switch(.data$type, 
         "occurrences-count" = collapse_occurrences_count(.data),
         "species-count" = collapse_species_count(.data),
         "species" = collapse_species(.data),
         "occurrences" = collapse_occurrences(.data),
         "media" = collapse_media_metadata(.data),
         abort("unrecognised 'type'"))
}

# if calling `collapse()` after `request_metadata()`
#' @rdname collect.data_request
#' @export
collapse.metadata_request <- function(.data){
  switch(.data$type,
         "fields" = collapse_fields(.data), 
         "apis" = collapse_apis(.data),
         "assertions" = collapse_assertions(.data),
         "atlases" = collapse_atlases(.data),
         "collections" = collapse_collections(.data),
         "datasets" = collapse_datasets(.data),
         "fields" = collapse_fields(.data),
         "layers" = collapse_layers(.data),
         "licences" = collapse_licences(.data),
         "lists" = collapse_lists(.data),
         "profiles" = collapse_profiles(.data),
         "providers" = collapse_providers(.data),
         "ranks" = collapse_ranks(.data),
         "reasons" = collapse_reasons(.data),
         "taxa" = collapse_taxa(.data))
}

# if calling `collapse()` after `request_files()`
#' @rdname collect.data_request
#' @export
collapse.files_request <- function(.data){
  switch(.data$type,
         "doi" = collapse_doi(.data),
         "distributions" = collapse_distribtions(.data),
         "media" = collapse_media_files(.data)
  )
}
         
#' Internal function to build headers at the `collapse()` stage
#' @noRd
#' @keywords Internal
build_headers <- function(){
  if(pour("atlas", "acronym") == "ALA"){
   list(
      "User-Agent" = galah_version_string(),
      "x-api-key" = pour("user", "api_key"))
  }else{
    list("User-Agent" = galah_version_string())
  }
}