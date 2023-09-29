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
  # get basic description of `query_set` object
  n <- length(.data)
  names_vec <- unlist(lapply(.data, function(a){a$type}))
  # look for any `data`
  data_lookup <- grepl("^data", names_vec)
  if(any(data_lookup)){
    data_names <- names_vec[data_lookup]
    # parse any `metadata`
    metadata_lookup <- grepl("^metadata", names_vec) &
                       !grepl("-unnest$", names_vec) # unnest functions only parse in collect()
    if(any(metadata_lookup)){
      metadata_names <- names_vec[metadata_lookup]
      metadata_results <- lapply(.data[metadata_lookup], collect)
      names(metadata_results) <- metadata_names    
    }else{
      metadata_results <- NULL
    }
    # now compute `data`
    if(any(names_vec == "data/media")){
      # exception here if media is requested
      # this is important because media is the _only_ case where "data" (i.e.
      # occurrences) has to be cached _prior_ to compute() of another "data" call
      occ_query <- .data[[which(names_vec == "data/occurrences")]]
      media_cols <- check_media_cols_present(occ_query)
      occ <- occ_query |>
        add_metadata(metadata_results) |>
        compute() |>
        collect(wait = TRUE)
      if(nrow(occ) < 1){
        abort("No occurrences returned")
      }else{
        data_tr <- .data[[which(names_vec == "data/media")]]
        data_tr$"data/occurrences" <- occ
        compute(data_tr)
      }
    }else{
      # parse `data`, including supplied metadata
      # this assumes only one `data` field is available per `query_set`
      .data[[which(data_lookup)]] |>
        add_metadata(metadata_results) |>
        compute()      
    }
  # FIXME: need to add `else if` here to account for `unnest` functions that require lookups
    # metadata/fields-unnest calls check_fields(), requiring fields and assertions
    # metadata/profiles-unnest calls profile_short_name(), which requires profiles
  # if no metadata are needed, return .data unaltered
  }else{ 
    compute(.data[[1]])
  }
}

# if calling `compute()` on an object extracted from `collapse()` 
#' @rdname collect.query
#' @export
compute.query <- function(.data, inputs = NULL){
  # (most) "data/" functions require pre-processing of metadata
  if(grepl("^data/", .data$type)){
    if(.data$type != "data/media"){ # these steps have already been completed for `media`
      .data <- .data |>
        check_login() |>
        check_reason() |>
        check_identifiers() |>
        check_fields() |>
        remove_metadata()
    }
    switch(.data$type, 
           "data/media" = compute_media(.data),
           "data/occurrences" = compute_occurrences(.data),
           "data/occurrences-count-groupby" = compute_occurrences_count(.data),
           "data/occurrences-count" = compute_occurrences_count(.data),
           "data/species-count" = compute_species_count(.data),
           .data)
  }else{
    switch(.data$type,
           # "-unnest" functions require some checks
           "metadata/fields-unnest" = check_fields(.data),
           "metadata/profiles-unnest" = compute_profile_values(.data),  # check this
           # some "metadata/" functions require pagination under some circumstances
           "metadata/collections" = compute_collections(.data),
           # "metadata/datasets" = compute_datasets(.data),
           "metadata/lists" = compute_lists(.data), # always paginates
           # "metadata/providers" = compute_providers(.data),
           .data # remaining "metadata/" functions are passed as-is
           )
  } 
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

## BELOW HERE PROBABLY WON'T WORK

# if calling `compute()` after `request_files()` 
#' @rdname collect.data_request
#' @export
compute.files_request <- function(.data){
  result <- collapse(.data)
  check_login(result)
  class(result) <- "files_response"
  return(result)
}

# if calling `compute()` after `collapse()` after `request_files()` 
#' @rdname collect.data_request
#' @export
compute.files_query <- function(.data){
  check_login(.data)
  class(.data) <- "files_response"
  return(.data)
}
