#' Internal function to build headers at the `collapse()` stage
#' @noRd
#' @keywords Internal
build_headers <- function(){
  list("User-Agent" = galah_version_string())
  ## Below code adds API keys (not yet implemented)
  ## Add these as optional
  # if(pour("atlas", "acronym") == "ALA"){
  #  list(
  #     "User-Agent" = galah_version_string(),
  #     "x-api-key" = pour("user", "api_key")
  ## if(){ # something about if we're using JWT tokens
  ##  list(
  ##    "User-Agent" = galah_version_string(),
  ##.   "Authorization" = paste("Bearer", access_token))
  ## }
  # }else{
  #   list("User-Agent" = galah_version_string())
  # }
}

#' Build query list from constituent arguments
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
build_query <- function(identify = NULL, 
                        filter = NULL, 
                        location = NULL, 
                        data_profile = NULL) {
  if(is.null(identify)) {
    taxa_query <- NULL
  } else { # assumes a tibble or data.frame has been given
    if(nrow(identify) < 1){
      taxa_query <- NULL
    } else {
      taxa_query <- "`TAXON_PLACEHOLDER`"
    }
  }
  # validate filters
  if (is.null(filter)) {
    filter_query <- NULL
  } else {
    if(!inherits(filter, "data.frame")){
      abort("`filter` must be a `data.frame` or `tibble`")
    }
    if (nrow(filter) == 0) {
      filter_query <- NULL
    } else {
      queries <- unique(filter$query)
      filter_query <- paste0(queries, collapse = " AND ")
    }
  }
  # merge
  query <- list(fq = c(taxa_query, filter_query)) 
  # geographic stuff
  if (!is.null(location)) {
    query$wkt <- location
  }
  # add profiles information (ALA only)  
  if(pour("atlas", "region") == "Australia"){
    if(!is.null(data_profile)) {
      query$qualityProfile <- data_profile
    } else {
      query$disableAllQualityFilters <- "true"
    }
  }
  build_single_fq(query)
}

#' Build query list from constituent arguments for GBIF only
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
build_query_gbif <- function(identify = NULL, 
                             filter = NULL){
  if(is.null(identify)) {
    taxa_query <- list(taxonKey = 1)
  }else{
    taxa_query <- list(taxonKey = "`TAXON_PLACEHOLDER`")
  }
  # filter
  if(is.null(filter)) {
    filter_query <- NULL
  }else{
    if(!inherits(filter, "data.frame")){
      abort("`filter` must be a `data.frame` or `tibble`")
    }
    if(nrow(filter) == 0) {
      filter_query <- NULL
    }else{
      filter_query <- build_filter_query(filter)
    }
  }
  # return merged output
  c(taxa_query, filter_query)
}

#' collapse multiple fq args into one
#' @keywords Internal
#' @noRd
build_single_fq <- function(query){
  if(any(names(query) == "fq")){
    # ensure all arguments from galah_filter are enclosed in brackets
    fq <- query$fq
    missing_brackets <- !grepl("^\\(", fq)
    if(any(missing_brackets)){
      fq[missing_brackets] <- paste0("(", fq[missing_brackets], ")")
    }
    fq_single <- paste(fq, collapse = "AND")
    c(fq = fq_single, query[names(query) != "fq"])
  }else{
    query
  }
}

#' Sub-function to `build_query()` for filters
#' only called by GBIF
#' @noRd
#' @keywords Internal
build_filter_query <- function(filters) {
  is_equals <- filters$logical == "=="
  if(any(is_equals)){
    filters$query[is_equals] <- filters$value[is_equals]
  }
  if(any(!is_equals)){
    filters$query[!is_equals] <- sub("^[[:graph:]]+\\[", 
                                     "", 
                                     x = filters$query[!is_equals]) |>
      sub("\\]$", "", x = _) |>
      sub(" TO ", ",", x = _)
  }
  queries <- as.list(filters$query)
  names(queries) <- filters$variable
  queries
}

#' Sub-function to `build_query()` for taxa
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @noRd
#' @keywords Internal
build_taxa_query <- function(ids) {
  ids <- ids[order(ids)]
  if(is_gbif()){
    list(taxonKey = ids)
  }else{
    wrapped_ids <- paste0("\"", ids, "\"")
    id_tag <- "lsid"
    glue(
      "({id_tag}:",
      glue_collapse(wrapped_ids,
                    sep = glue(" OR {id_tag}:")),
      ")")
  }
}

#' Sub-function to convert assertions to logicals in `collect_occurrences()`
#' @noRd
#' @keywords Internal
fix_assertion_cols <- function(df, assertion_cols) {
  for (col in assertion_cols) {
    df[[col]] <- as.logical(df[[col]])
  }
  df
}

#' Internal function to handle APIs that return complex outputs
#' Currently only used by `collect_collection_values()`
#' It is pretty messy, as:
#'  1. ALA returns empty lists and NULL values in some fields, and 
#'  2. tibble() and friends don't handle list-columns well
#' @importFrom tibble as_tibble
#' @noRd
#' @keywords Internal
build_tibble_from_nested_list <- function(result){
  # handle normal columns
  source_tibble <- lapply(result, function(a){
    if(is.null(a)){
      as.character(NA)
    }else if(length(a) > 1){
      as.character(NA)
    }else if(length(a) < 1){
      as.character(NA)
    }else{
      a
    }
  }) |>
    as_tibble()
  # handle nested columns
  list_cols <- lapply(result, 
                      function(a){is.list(a) & length(a) > 0}) |>
    unlist()
  if(any(list_cols)){
    list_data <- result[list_cols]
  }else{
    list_data <- NULL
  }
  # stick together
  if(length(list_data) > 0){
    for(i in seq_along(list_data)){
      col <- names(list_data)[i]
      source_tibble[col][[1]] <- list(list_data[col][[1]])
    }
  }
  return(source_tibble)
}

#' Build a valid wkt string from a spatial polygon
#' Internal function to `galah_bbox` and `galah_polygon()`
#' @importFrom sf st_as_text
#' @importFrom sf st_cast
#' @importFrom sf st_geometry
#' @importFrom sf st_geometry_type
#' @importFrom sf st_is_simple
#' @noRd
#' @keywords Internal
build_wkt <- function(polygon, error_call = caller_env()) {
  if (st_geometry_type(polygon) == "POLYGON") {
    polygon <- st_cast(polygon, "MULTIPOLYGON")
  }
  if (!st_is_simple(polygon)) {
    bullets <- c(
      "The area provided to `galah_bbox` is too complex. ",
      i = "See `?sf::st_simplify` for how to simplify geospatial objects."
    )
    abort(bullets, call = caller_env())
  }
  wkt <- st_as_text(st_geometry(polygon))
  wkt
}