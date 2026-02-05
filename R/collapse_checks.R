#' Internal function to build necessary metadata into a single object
#' I.e. to parse a `query_set` object within `collapse()`
#' @noRd
#' @keywords Internal
collapse_build_checks <- function(.query){
  # get basic description of `query_set` object
  n <- length(.query)
  names_vec <- purrr::map(.query,
                          \(a){purrr::pluck(a, "type")}) |>
    unlist()
  data_lookup <- stringr::str_detect(names_vec, "^data")
  if(any(data_lookup)){
    data_names <- names_vec[data_lookup]
    # parse any `metadata`
    metadata_results <- collapse_run_metadata(names_vec, .query)
    # parse `data`, including supplied metadata
    # this assumes only one `data` field is available per `query_set`
    .query[[which(data_lookup)]] |>
      collapse_add_metadata(metadata_results)
  }else if(any(names_vec %in% c("metadata/fields-unnest", 
                                "metadata/profiles-unnest",
                                "metadata/taxa-unnest"))){
    # this code accounts for `unnest` functions that require lookups
    # metadata/fields-unnest calls check_fields(), requiring fields and assertions
    # metadata/profiles-unnest calls profile_short_name(), which requires profiles
    if(length(.query) > 1){
      metadata_results <- collapse_run_metadata(names_vec, .query)
      .query[[2]] |>
        collapse_add_metadata(metadata_results)
    }else{
      .query[[1]]
    }
  }else{ 
    # if no metadata are needed, return .query unaltered
    .query[[1]]
  }
}

#' Internal function to run metadata checks
#' This is useful for testing, particularly in testing `galah_select()`
#' called by `collapse()`
#' @noRd
#' @keywords Internal
collapse_run_checks <- function(.query,
                                error_call = rlang::caller_env()){
  # "data/" functions require pre-processing of metadata,
  if(stringr::str_detect(.query$type, "^data/")){
    # taxon concept ID must always be evaluated
    .query <- check_identifiers(.query, error_call) 
    # login should only be evaluated for species and occurrence
    if(.query$type %in% c("data/occurrences", "data/species")){
      .query <- check_login(.query, error_call)
    }
    # check_select() is specifically for parsing fields into urls,
    # should only be called for occurrences
    if(.query$type %in% c("data/occurrences", "data/occurrences-glimpse")){
      .query <- check_select(.query, error_call)
    }

    # after checking, for type = "glimpse", we need to rename the fields query
    if(.query$type == "data/occurrences-glimpse"){
      url <- httr2::url_parse(.query$url)
      query_names <- names(url$query)
      if(any(query_names == "fields")){
        names(url$query)[which(query_names == "fields")] <- "fl"
      }
      .query$url <- httr2::url_build(url)
    } 

    # run remaining checks, if requested by the user
    if(potions::pour("package", "run_checks")) {
      .query <- .query |>
        check_reason(error_call) |>
        check_fields(error_call) |>
        check_profiles(error_call)
    }
  # as do `unnest()`/`show_values()` functions
  }else if(stringr::str_detect(.query$type, "-unnest$")){
    # FIXME: decide which checks should be subject to `if(potions::pour("package", "run_checks"))`
    .query <- .query |>
      check_identifiers() |>
      check_fields()
  }
  collapse_remove_metadata(.query)
  # special cases:
  # distributions
  # if(.query$type == "data/distributions" & 
  #    !is.null(.query[["metadata/distributions"]])){
  #   .query$url <- tibble(url = glue(utils::URLdecode(.query$url), 
  #                                   id = .query[["metadata/distributions"]]$id))
  # }
}

#' Internal function to collapse metadata
#' @noRd
#' @keywords Internal
collapse_run_metadata <- function(names_vec, .query){
  metadata_lookup <- grepl("^metadata", names_vec) &
    !grepl("-unnest$", names_vec) # unnest functions only parse in collect()
  if(any(metadata_lookup)){
    metadata_names <- names_vec[metadata_lookup]
    metadata_results <- purrr::map(.query[which(metadata_lookup)], collect)
    names(metadata_results) <- metadata_names   
    metadata_results
  }else{
    NULL
  }
}

#' Internal function to pass metadata to `collapse()` functions
#' called by `compute.query_set()`
#' @noRd
#' @keywords Internal
collapse_add_metadata <- function(query, meta){
  c(query, meta) |>
    as_query()
}

#' Internal function to reduce size of internally computed objects
#' called by `compute.query()`
#' @noRd
#' @keywords Internal
collapse_remove_metadata <- function(.query){
  names_lookup <- stringr::str_detect( names(.query), "^metadata/")
  if(any(names_lookup)){
    as_query(.query[!names_lookup])
  }else{
    as_query(.query)
  }
}