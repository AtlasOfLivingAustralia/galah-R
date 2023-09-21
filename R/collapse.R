# if calling `collapse()` after `request_data()`
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @rdname collect.data_request
#' @export
collapse.data_request <- function(.data, mint_doi = FALSE){
  # .data$type <- check_type(.data$type) # needed?
  
  # handle `run_checks`
  if(pour("package", "run_checks")){
    result <- list(
      collapse_fields(),
      collapse_assertions())
  }else{
    result <- list()
  }
  
  # handle `identify()`
  if(!is.null(.data$identity)){
    first_col <- colnames(.data$identify)[1]
    result <- c(
      result,
      switch(first_col, 
             "search_term" = collapse_taxa_single(.data),
             "identifier"  = collapse_identifiers(.data),
             collapse_taxa_multiple(.data)))
  }

  # handle query
  result[[(length(result) + 1)]] <- switch(
    .data$type,
    "doi" = collapse_doi(.data),
    "media" = collapse_media_metadata(.data),
    "occurrences" = collapse_occurrences(.data, mint_doi = mint_doi),
    "occurrences-count" = collapse_occurrences_count(.data),
    "species" = collapse_species(.data),
    "species-count" = collapse_species_count(.data),
    abort("unrecognised 'type'"))
  
  class(result) <- "query_set"
  result
}

# if calling `collapse()` after `request_metadata()`
#' @rdname collect.data_request
#' @export
collapse.metadata_request <- function(.data){
  result <- list(switch(.data$type,
         "apis" = collapse_apis(),
         "assertions" = collapse_assertions(),
         "atlases" = collapse_atlases(),
         "collections" = collapse_collections(),
         "datasets" = collapse_datasets(),
         "fields" = collapse_fields(),
         "licences" = collapse_licences(),
         "lists" = collapse_lists(),
         "profiles" = collapse_profiles(),
         "providers" = collapse_providers(),
         "ranks" = collapse_ranks(),
         "reasons" = collapse_reasons(),
         "taxa" = collapse_taxa(.data),
         "identifiers" = collapse_identifiers(.data),
         abort("unrecognised 'type'"))
  )
  class(result) <- "query_set"
  result
}

# if calling `collapse()` after `request_values()`
#' @rdname collect.data_request
#' @export
collapse.values_request <- function(.data){
  .data <- check_values_filter(.data)
  switch(.data$type,
         "collections" = collapse_collection_values(.data),
         "datasets" = collapse_dataset_values(.data),
         "fields" = collapse_field_values(.data),
         "lists" = collapse_list_values(.data),
         "profiles" = collapse_profile_values(.data),
         "providers" = collapse_provider_values(.data),
         "taxa" = collapse_taxa_values(.data),
         abort("unrecognised 'type'"))
}

# if calling `collapse()` after `request_files()`
#' @rdname collect.data_request
#' @export
collapse.files_request <- function(.data, 
                                   # prefix? could be useful for file names
                                   thumbnail = FALSE
                                   ){
  switch(.data$type,
         "distributions" = collapse_distribtions(.data),
         "media" = collapse_media_files(.data, thumbnail = thumbnail)
  )
}

#' Internal function to `collapse.values_request()` functions
#' @noRd
#' @keywords Internal
check_values_filter <- function(.data){
  if(is.null(.data$filter)){
    bullets <- c("`collapse.values_request()` requires a `filter()` argument",
                 i = "e.g. `request_values() |> filter(field == basisOfRecord) |> collapse()`")
    abort(bullets, call = caller_env())
  }else{
    if(!grepl("s$|taxa", .data$filter$api)){
      api <- paste0(.data$filter$api, "s")
      .data$filter$api <- api
      .data$type <- api
    }else{
      .data$type <- .data$filter$api
    }
    return(.data)
  }
}