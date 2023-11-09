#' @title Compute a query
#' @description `compute()` is useful for several purposes. It's original 
#' purpose is to send a request for data, which can then be processed by the 
#' server and retrieved at a later time (via `collect()`). However, because 
#' query-altering functions (such as `filter()` and `select()`) are evaluated 
#' lazily from galah version 2.0 onwards, `compute()` is also the function where
#' all objects within the `query_set` are evaluated, and any checks run using
#' that information. Therefore it is possible for invalid queries to be built 
#' using `collapse()`, but fail at `compute()`.
#' @name compute_galah
#' @order 1
#' @param x An object of class `data_request`, `metadata_request` or 
#' `files_request` (i.e. constructed using a pipe) or `query` or `query_set`
#' (i.e. constructed by `collapse()`) 
#' @param ... Arguments passed on to other methods
#' @return An object of class `query` containing a checked, valid query
#' for the selected atlas. In the case of occurrence data, also contains
#' information on the status of the request.
#' @export
compute.data_request <- function(x, ...){
  # x$type <- check_type(x$type) # possibly still needed; unclear
  collapse(x, ...) |> compute()
}

# if calling `compute()` after `request_metadata()` 
#' @rdname compute_galah
#' @order 2
#' @export
compute.metadata_request <- function(x, ...){
  collapse(x, ...) |> compute()
}

# if calling `compute()` after `request_files()` 
#' @rdname compute_galah
#' @order 3
#' @export
compute.files_request <- function(x, ...){
  result <- collapse(x, ...)
  result[[1]]
}

# if calling `compute()` after `collapse()`
#' @rdname compute_galah
#' @order 4
#' @export
compute.query_set <- function(x, ...){
  build_checks(x) |> compute()
}

# if calling `compute()` on an object extracted from `collapse()` 
#' @rdname compute_galah
#' @order 5
#' @export
compute.query <- function(x, ...){
  x <- compute_checks(x)
  switch(x$type, 
         "data/occurrences" = compute_occurrences(x),
         "data/occurrences-count-groupby" = compute_occurrences_count(x),
         "data/occurrences-count" = compute_occurrences_count(x),
         "data/species" = compute_species(x),
         "data/species-count" = compute_species_count(x),
         # "-unnest" functions require some checks
         "metadata/profiles-unnest" = compute_profile_values(x),  # check this
         # some "metadata/" functions require pagination under some circumstances
         "metadata/lists" = compute_lists(x), # always paginates
         x # remaining "metadata/" functions are passed as-is
  )
}

#' Internal function to build necessary metadata into a single object
#' This has been parsed out to support improved testing of `compute()` stages
#' @noRd
#' @keywords Internal
build_checks <- function(.query){
  # get basic description of `query_set` object
  n <- length(.query)
  names_vec <- unlist(lapply(.query, function(a){a$type}))
  # look for any `data`
  data_lookup <- grepl("^data", names_vec)
  if(any(data_lookup)){
    data_names <- names_vec[data_lookup]
    # parse any `metadata`
    metadata_results <- parse_metadata(names_vec, .query)
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
      metadata_results <- parse_metadata(names_vec, .query)
      .query[[2]] |>
        add_metadata(metadata_results)
    }else{
      .query[[1]]
    }
  }else{ 
    # if no metadata are needed, return .query unaltered
    .query[[1]]
  }
}

#' Internal function to parse metadata
#' @noRd
#' @keywords Internal
parse_metadata <- function(names_vec, .query){
  metadata_lookup <- grepl("^metadata", names_vec) &
    !grepl("-unnest$", names_vec) # unnest functions only parse in collect()
  if(any(metadata_lookup)){
    metadata_names <- names_vec[metadata_lookup]
    metadata_results <- lapply(.query[metadata_lookup], collect)
    names(metadata_results) <- metadata_names   
    metadata_results
  }else{
    NULL
  }
}

#' Internal function to run metadata checks
#' This is useful for testing, particularly in testing `galah_select()`
#' called by `compute.query_set()`
#' @noRd
#' @keywords Internal
compute_checks <- function(.query){
  # "data/" functions require pre-processing of metadata,
  # as do `unnest()`/`show_values()` functions
  if(grepl("^data/", .query$type) |
     grepl("-unnest$", .query$type)
  ){
    # some checks should happen regardless of `run_checks`
    .query <- .query |>
      check_identifiers() |> 
      check_select()
    if(pour("package", "run_checks")) {
      .query <- .query |>
        check_login() |>
        check_reason() |>
        check_fields() |>
        check_profiles()
    }
    .query <- remove_metadata(.query)
  }
  .query
}

#' Internal function to pass metadata to `compute()` functions
#' called by `compute.query_set()`
#' @noRd
#' @keywords Internal
add_metadata <- function(query, meta){
  result <- c(query, meta)
  class(result) <- "query"
  return(result)
}

#' Internal function to reduce size of internally computed objects
#' called by `compute.query()`
#' @noRd
#' @keywords Internal
remove_metadata <- function(.query){
  names_lookup <- grepl("^metadata/", names(.query))
  if(any(names_lookup)){
    x <- .query[!names_lookup]
  }else{
    x <- .query
  }
  class(x) <- "query"
  x
}
