# NOTE: compute is where queries can be cached with the specified atlas;
# but also where checks are run to ensure later queries are valid

# if calling `compute()` after `request_data()` 
#' @rdname collect.query
#' @export
compute.data_request <- function(.data){
  # .data$type <- check_type(.data$type) # possibly still needed; unclear
  collapse(.data) |> compute()
}

# if calling `compute()` after `request_metadata()` 
#' @rdname collect.query
#' @export
compute.metadata_request <- function(.data){
  collapse(.data) |> compute()
}

# if calling `compute()` after `collapse()`
#' @rdname collect.query
#' @export
compute.query_set <- function(.data){
  build_checks(.data) |> compute()
}

#' Internal function to build necessary metadata into a single object
#' This has been parsed out to support improved testing of `compute()` stages
#' @noRd
#' @keywords Internal
build_checks <- function(.data){
  # get basic description of `query_set` object
  n <- length(.data)
  names_vec <- unlist(lapply(.data, function(a){a$type}))
  # look for any `data`
  data_lookup <- grepl("^data", names_vec)
  if(any(data_lookup)){
    data_names <- names_vec[data_lookup]
    # parse any `metadata`
    metadata_results <- parse_metadata(names_vec, .data)
    # parse `data`, including supplied metadata
    # this assumes only one `data` field is available per `query_set`
    .data[[which(data_lookup)]] |>
      add_metadata(metadata_results)     
  }else if(any(names_vec %in% c("metadata/fields-unnest", 
                                "metadata/profiles-unnest",
                                "metadata/taxa-unnest"))){
    # this code accounts for `unnest` functions that require lookups
    # metadata/fields-unnest calls check_fields(), requiring fields and assertions
    # metadata/profiles-unnest calls profile_short_name(), which requires profiles
    if(length(.data) > 1){
      metadata_results <- parse_metadata(names_vec, .data)
      .data[[2]] |>
        add_metadata(metadata_results)
    }else{
      .data[[1]]
    }
  }else{ 
    # if no metadata are needed, return .data unaltered
    .data[[1]]
  }
}

#' Internal function to parse metadata
#' @noRd
#' @keywords Internal
parse_metadata <- function(names_vec, .data){
  metadata_lookup <- grepl("^metadata", names_vec) &
    !grepl("-unnest$", names_vec) # unnest functions only parse in collect()
  if(any(metadata_lookup)){
    metadata_names <- names_vec[metadata_lookup]
    metadata_results <- lapply(.data[metadata_lookup], collect)
    names(metadata_results) <- metadata_names   
    metadata_results
  }else{
    NULL
  }
}

# if calling `compute()` on an object extracted from `collapse()` 
#' @rdname collect.query
#' @export
compute.query <- function(.data){
  .data <- compute_checks(.data)
  switch(.data$type, 
         "data/occurrences" = compute_occurrences(.data),
         "data/occurrences-count-groupby" = compute_occurrences_count(.data),
         "data/occurrences-count" = compute_occurrences_count(.data),
         "data/species" = compute_species(.data),
         "data/species-count" = compute_species_count(.data),
         # "-unnest" functions require some checks
         "metadata/profiles-unnest" = compute_profile_values(.data),  # check this
         # some "metadata/" functions require pagination under some circumstances
         "metadata/lists" = compute_lists(.data), # always paginates
         .data # remaining "metadata/" functions are passed as-is
  )
}

#' Internal function to run metadata checks
#' This is useful for testing, particularly in testing `galah_select()`
#' called by `compute.query_set()`
#' @noRd
#' @keywords Internal
compute_checks <- function(.data){
  # "data/" functions require pre-processing of metadata,
  # as do `unnest()`/`show_values()` functions
  if(grepl("^data/", .data$type) |
     grepl("-unnest$", .data$type)
  ){
    # some checks should happen regardless of `run_checks`
    .data <- .data |>
      check_identifiers() |> 
      check_select()
    if(pour("package", "run_checks")) {
      .data <- .data |>
        check_login() |>
        check_reason() |>
        check_fields() |>
        check_profiles()
    }
    .data <- remove_metadata(.data)
  }
  .data
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
remove_metadata <- function(.data){
  names_lookup <- grepl("^metadata/", names(.data))
  if(any(names_lookup)){
    x <- .data[!names_lookup]
  }else{
    x <- .data
  }
  class(x) <- "query"
  x
}

# if calling `compute()` after `request_files()` 
#' @rdname collect.data_request
#' @export
compute.files_request <- function(.data){
  result <- collapse(.data)
  result[[1]]
}

# if calling `compute()` after `collapse()` after `request_files()` 
#' @rdname collect.data_request
#' @export
compute.files_query <- function(.data){
  .data[[1]]
}
