#' Internal function to build necessary metadata into a single object
#' I.e. to parse a `query_set` object within `collapse()`
#' @noRd
#' @keywords Internal
collapse_build_checks <- function(.query){
  # get basic description of `query_set` object
  n <- length(.query)
  names_vec <- unlist(lapply(.query, function(a){a$type}))
  # look for any `data`
  data_lookup <- grepl("^data", names_vec)
  if(any(data_lookup)){
    data_names <- names_vec[data_lookup]
    # parse any `metadata`
    metadata_results <- collapse_parse_metadata(names_vec, .query)
    # parse `data`, including supplied metadata
    # this assumes only one `data` field is available per `query_set`
    .query[[which(data_lookup)]] |>
      add_metadata(metadata_results)
  }else if(any(names_vec %in% c("metadata/fields-unnest", 
                                "metadata/profiles-unnest",
                                "metadata/taxa-unnest"))){
    # this code accounts for `unnest` functions that require lookups
    # metadata/fields-unnest calls check_fields(), requiring fields and assertions
    # metadata/profiles-unnest calls profile_short_name(), which requires profiles
    if(length(.query) > 1){
      metadata_results <- collapse_parse_metadata(names_vec, .query)
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
collapse_run_checks <- function(.query){
  # "data/" functions require pre-processing of metadata,
  # as do `unnest()`/`show_values()` functions
  if(grepl("^data/", .query$type) |
     grepl("-unnest$", .query$type)){
    # some checks should happen regardless of `run_checks`
    .query <- .query |>
      check_identifiers() |> 
      check_select()
    if(potions::pour("package", "run_checks")) {
      .query <- .query |>
        check_login() |>
        check_reason() |>
        check_fields() |>
        check_profiles()
    }
    # special cases:
    # distributions
    if(.query$type == "data/distributions" & 
       !is.null(.query[["metadata/distributions"]])){
      .query$url <- tibble(url = glue(utils::URLdecode(.query$url), 
                                      id = .query[["metadata/distributions"]]$id))
    }
    # GBIF predicates:
    if(any(names(.query) == "predicates")){
      result <- .query |>
        purrr::pluck("predicates") |>
        build_predicates()
      .query$predicates <- result
    }
    # TODO: might need to promote this up a bit
    # the problem is that we need to add function-specific content
    # but that content isn't available here
    
    # clean up
    .query <- collapse_remove_metadata(.query)
  }
  .query
}

#' Internal function to collapse metadata
#' @noRd
#' @keywords Internal
collapse_metadata <- function(names_vec, .query){
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
  result <- c(query, meta)
  class(result) <- "query"
  return(result)
}

#' Internal function to reduce size of internally computed objects
#' called by `compute.query()`
#' @noRd
#' @keywords Internal
collapse_remove_metadata <- function(.query){
  names_lookup <- grepl("^metadata/", names(.query))
  if(any(names_lookup)){
    x <- .query[!names_lookup]
  }else{
    x <- .query
  }
  class(x) <- "query"
  x
}