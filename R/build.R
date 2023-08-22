#' Build query list from constituent arguments
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
build_query <- function(identify, 
                        filter, 
                        location = NULL, 
                        profile = NULL) {
  
  if (is.null(identify)) {
    if(is_gbif()){
      taxa_query <- list(taxonKey = 1)
    }else{
      taxa_query <- NULL
    }
  } else { # assumes a tibble or data.frame has been given
    if(nrow(identify) < 1){
      taxa_query <- NULL
    } else {
      check_taxa_arg(identify)
      if (inherits(identify, "data.frame") &&
          "identifier" %in% colnames(identify)) {
        identify <- identify$identifier
      }
      #TODO: Implement a useful check here- i.e. string or integer
      taxa_query <- build_taxa_query(identify)
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
      filter_query <- build_filter_query(filter)
    }
  }
  
  if(is_gbif()){
    query <- c(taxa_query, filter_query)
  }else{
    query <- list(fq = c(taxa_query, filter_query)) 
  } 
  
  # geographic stuff
  if (!is.null(location)) {
    query$wkt <- location
  }
  
  # add profiles information (ALA only)  
  if(pour("atlas", "region") == "Australia"){
    if(!is.null(profile)) {
      query$qualityProfile <- profile
    } else {
      query$disableAllQualityFilters <- "true"
    }
  }
  
  build_single_fq(query)
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
#' @noRd
#' @keywords Internal
build_filter_query <- function(filters) {
  if(is_gbif()){
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
  }else{
    queries <- unique(filters$query)
    paste0(queries, collapse = " AND ")
  }
}

#' Sub-function to `build_query()` for taxa
#' @noRd
#' @keywords Internal
build_taxa_query <- function(ids) {
  ids <- ids[order(ids)]
  if(is_gbif()){
    list(taxonKey = ids)
  }else{
    glue(
      "(lsid:",
      glue_collapse(ids, sep = glue(" OR lsid:")),
      ")")
  }
}

#' Sub-function to build a column name string to support `select.data_request()`
#' @noRd
#' @keywords Internal
build_columns <- function(col_df) {
  if (nrow(col_df) == 0) {
    return("")
  }
  paste0(col_df$name, collapse = ",")
}

#' Sub-function to build assertions in `collapse_occurrences()`
#' @noRd
#' @keywords Internal
build_assertion_columns <- function(col_df) {
  assertion_group <- any(attr(col_df, "group") == "assertions")
  assertion_rows <- which(col_df$type == "assertion")
  if(assertion_group){ # assertions have been selected as a group
    if(length(assertion_rows) > 50){ # only if a certain number present
      return("includeall")
    }else{
      return(paste0(col_df$name[assertion_rows], collapse = ","))
    }
  }else{ # assertions not selected as a group
    if(length(assertion_rows) > 0) {
      return(paste0(col_df$name[assertion_rows], collapse = ","))
    }else{
      return("none")
    }
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