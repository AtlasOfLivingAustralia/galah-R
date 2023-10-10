# if calling `collapse()` after `request_data()`
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @rdname collect.query
#' @export
collapse.data_request <- function(.data, mint_doi = FALSE){
  # .data$type <- check_type(.data$type) # needed?
  # handle `run_checks`
  if (pour("package", "run_checks")) {
    # add check here to see whether any filters are specified
    # it is possible to only call `identify()`, for example
    fields_absent <- lapply(
      .data[c("arrange", "filter", "select", "group_by")],
      is.null
    ) |>
      unlist()
    if (any(!fields_absent) | .data$type == "species-count") {
      result <- list(collapse_fields(), collapse_assertions())
    } else {
      if(!is.null(.data$identify)){
        result <- list(collapse_fields())
      }else{
        result <- list()
      }
    }
    if (.data$type %in% c("occurrences", "media", "species") &
        atlas_supports_reasons_api()) {
      result[[(length(result) + 1)]] <- collapse_reasons()
    }
  } else {
    result <- list()
  }
  # handle `identify()`
  if(!is.null(.data$identify)){
    result[[(length(result) + 1)]] <- collapse_taxa(list(identify = .data$identify))
  }
  if(.data$type == "media"){
    # occurrences are a pre-condition to media
    result[[(length(result) + 1)]] <- collapse_occurrences(.data, mint_doi = mint_doi)
  }
  # handle query
  result[[(length(result) + 1)]] <- switch(
    .data$type,
    "doi" = collapse_doi(.data),
    "media" = collapse_media(.data),
    "occurrences" = collapse_occurrences(.data, mint_doi = mint_doi),
    "occurrences-count" = collapse_occurrences_count(.data),
    "species" = collapse_species(.data),
    "species-count" = collapse_species_count(.data),
    abort("unrecognised 'type'"))
    class(result) <- "query_set"
  result
}

# if calling `collapse()` after `request_metadata()`
#' @rdname collect.query
#' @export
collapse.metadata_request <- function(.data){
  if(pour("package", "run_checks")){
    result <- switch(.data$type, 
                     "fields-unnest" = list(collapse_fields()),
                     "profiles-unnest" = list(collapse_profiles()),
                     list())
  }else{
    result <- list()
  }
  result[[(length(result) + 1)]] <- switch(.data$type,
         "apis" = collapse_apis(),
         "assertions" = collapse_assertions(),
         "atlases" = collapse_atlases(),
         "collections" = collapse_collections(.data),
         "datasets" = collapse_datasets(.data),
         "fields" = collapse_fields(),
         "fields-unnest" = collapse_fields_unnest(.data),
         "licences" = collapse_licences(),
         "lists" = collapse_lists(.data),
         "lists-unnest" = collapse_lists_unnest(.data),
         "profiles" = collapse_profiles(),
         "profiles-unnest" = collapse_profiles_unnest(.data),
         "providers" = collapse_providers(.data),
         "ranks" = collapse_ranks(),
         "reasons" = collapse_reasons(),
         "taxa" = collapse_taxa(.data),
         "taxa-unnest" = collapse_taxa_unnest(.data),
         "identifiers" = collapse_identifiers(.data),
         abort("unrecognised 'type'")
  )
  class(result) <- "query_set"
  result
}

# if calling `collapse()` after `request_files()`
#' @rdname collect.query
#' @export
collapse.files_request <- function(.data, 
                                   # prefix? could be useful for file names
                                   thumbnail = FALSE
                                   ){
  result <- list(switch(.data$type,
         # "distributions" = collapse_distribtions(.data),
         "media" = collapse_media_files(.data, thumbnail = thumbnail)
  ))
  class(result) <- "query_set"
  result
}