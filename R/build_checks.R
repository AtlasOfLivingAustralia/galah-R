#' Internal function to build necessary metadata into a single object
#' I.e. to parse a `query_set` object within `collapse()`
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

#' Internal function to pass metadata to `collapse()` functions
#' called by `compute.query_set()`
#' @noRd
#' @keywords Internal
add_metadata <- function(query, meta){
  result <- c(query, meta)
  class(result) <- "query"
  return(result)
}