#' @title Generate a query
#' @description `collapse()` constructs a valid query so it can be 
#' inspected before being sent. It typically occurs at the end of a pipe,
#' traditionally begun with `galah_call()`, that is used to define a query.
#' As of version 2.0, objects of class `data_request` (created using 
#' `request_data()`), `metadata_request` (from `request_metadata()`) or 
#' `files_request` (from `request_files()`) are all supported by `collapse()`. 
#' Any of these objects can be created using `galah_call()` via the `method`
#' argument.
#' @name collapse_galah
#' @order 1
#' @param x An object of class `data_request`, `metadata_request` or 
#' `files_request`
#' @param ... Arguments passed on to methods
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @return An object of class `query_set`, which is a list containing one or 
#' more objects of class `query`. This is valuable because it shows the set of 
#' queries required to correctly retrieve the requested data. Objects within a
#' `query_set` are listed in the sequence in which they will be enacted.
#' @export
collapse.data_request <- function(x, ..., mint_doi){
  if(!missing(mint_doi)){
    x$mint_doi <- mint_doi}
  # x$type <- check_type(x$type) # needed?
  # handle sending dois via `filter()`
  # important this happens first, as it affects `type` which affects later code
  variables <- x$filter$variable
  if(!is.null(variables)){
    if(length(variables) == 1 & variables[1] == "doi"){
      x$type <- "occurrences-doi"
    }
  }
  # handle `run_checks`
  fields_absent <- lapply(
    x[c("arrange", "filter", "select", "group_by")],
    is.null
  ) |>
    unlist()
  if (pour("package", "run_checks") & x$type != "occurrences-doi") {
    # add check here to see whether any filters are specified
    # it is possible to only call `identify()`, for example
    if (any(!fields_absent) | x$type == "species-count") {
      result <- list(collapse_fields(), collapse_assertions())
    } else {
      # for living atlases, we need `collapse_fields()` to check the `lsid` field
      # this isn't required for GBIF which doesn't use `fq` for taxon queries
      if(!is.null(x$identify) &!is_gbif()){
        result <- list(collapse_fields())
      }else{
        result <- list()
      }
    }
    if (x$type %in% c("occurrences", "media", "species") &
        atlas_supports_reasons_api()) {
      result[[(length(result) + 1)]] <- collapse_reasons()
    }
  } else { # if select is required, we need fields even if `run_checks == FALSE`
    if(!fields_absent[["select"]] | x$type == "occurrences"){
      result <- list(collapse_fields(), collapse_assertions())
    }else{
      result <- list() 
    }
  }
  # handle `identify()`
  if(!is.null(x$identify) & x$type != "occurrences-doi"){
    result[[(length(result) + 1)]] <- collapse_taxa(list(identify = x$identify))
  }
  # handle `apply_profile()`
  if(!is.null(x$data_profile)){
    result[[(length(result) + 1)]] <- collapse_profiles()
  }
  # handle query
  result[[(length(result) + 1)]] <- switch(
    x$type,
    "occurrences" = collapse_occurrences(x),
    "occurrences-count" = collapse_occurrences_count(x),
    "occurrences-doi" = collapse_occurrences_doi(x),
    "species" = collapse_species(x),
    "species-count" = collapse_species_count(x),
    abort("unrecognised 'type'"))
    class(result) <- "query_set"
  result
}

# if calling `collapse()` after `request_metadata()`
#' @rdname collapse_galah
#' @order 2
#' @export
collapse.metadata_request <- function(x, ...){
  if(pour("package", "run_checks")){
    result <- switch(x$type, 
                     "fields-unnest" = list(collapse_fields()),
                     "profiles-unnest" = list(collapse_profiles()),
                     list())
  }else{
    result <- list()
  }
  if(grepl("-unnest$", x$type)){
    if(x$type == "taxa-unnest"){
      # identify() calls must be parsed, irrespective of `run_checks` (which is parsed above)
      if(!is.null(x$identify)){
        result[[(length(result) + 1)]] <- collapse_taxa(x)
      }
      if(is.null(x$identify) & is.null(x$filter)){
        abort("Requests of type `taxa-unnest` must also supply one of `filter()` or `identify()`.")
      }
    }else if(is.null(x$filter)){
      current_type <- x$type
      bullets <- glue("Requests of type `{current_type}` containing `unnest` must supply `filter()`.")
      abort(bullets)
    }
  }
  result[[(length(result) + 1)]] <- switch(x$type,
         "apis" = collapse_apis(),
         "assertions" = collapse_assertions(),
         "atlases" = collapse_atlases(),
         "collections" = collapse_collections(x),
         "datasets" = collapse_datasets(x),
         "fields" = collapse_fields(),
         "fields-unnest" = collapse_fields_unnest(x),
         "licences" = collapse_licences(),
         "lists" = collapse_lists(x),
         "lists-unnest" = collapse_lists_unnest(x),
         "media" = collapse_media(x),
         "profiles" = collapse_profiles(),
         "profiles-unnest" = collapse_profiles_unnest(x),
         "providers" = collapse_providers(x),
         "ranks" = collapse_ranks(),
         "reasons" = collapse_reasons(),
         "taxa" = collapse_taxa(x),
         "taxa-unnest" = collapse_taxa_unnest(x),
         "identifiers" = collapse_identifiers(x),
         abort("unrecognised 'type'")
  )
  class(result) <- "query_set"
  result
}

# if calling `collapse()` after `request_files()`
#' @rdname collapse_galah
#' @order 3
#' @param thumbnail Logical: should thumbnail-size images be returned? Defaults 
#' to `FALSE`, indicating full-size images are required.
#' @export
collapse.files_request <- function(x,
                                   ...,
                                   # prefix? could be useful for file names
                                   thumbnail = FALSE
                                   ){
  result <- list(switch(x$type,
         "media" = collapse_media_files(x, thumbnail = thumbnail)
  ))
  class(result) <- "query_set"
  result
}
