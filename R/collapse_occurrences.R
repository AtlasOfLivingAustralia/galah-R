#' Internal function to `collapse()` for `type = "occurrences"`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_occurrences <- function(.query){
  if(is.null(.query$filter) & 
     is.null(.query$identify) & 
     is.null(.query$geolocate)){
    abort("No filters supplied to `collapse()` with `type = \"occurrences\"`")
  }
  switch(pour("atlas", "region"),
         "United Kingdom" = collapse_occurrences_uk(.query),
         "Global" = collapse_occurrences_gbif(.query),
         collapse_occurrences_la(.query))
}

#' calculate the query to be returned for the UK atlas
#' @param .query An object of class `data_request()`
#' @noRd
#' @keywords Internal
collapse_occurrences_uk <- function(.query){
  # set default columns
  if(is.null(.query$select)){
    .query$select <- galah_select(group = "basic")
  }
  # build a url
  # NOTE: providing an email blocks this from executing (2023-08-30)
  url <-  url_lookup("data/occurrences") |> 
    url_parse()
  url$query <- c(build_query(identify = .query$identify,
                             filter = .query$filter, 
                             location = .query$geolocate, 
                             data_profile = .query$data_profile),
                 fields = "`SELECT_PLACEHOLDER`",
                 qa = "`ASSERTIONS_PLACEHOLDER`",
                 sourceTypeId = 2001,
                 fileType = "csv",
                 reasonTypeId = pour("user", "download_reason_id"),
                 dwcHeaders = "true")
  # build output
  result <- list(
    type = "data/occurrences",
    url = url_build(url),
    headers = build_headers(),
    select = .query$select)
  class(result) <- "query"
  return(result)
}

#' calculate the query to be returned for GBIF
#' @noRd
#' @keywords Internal
collapse_occurrences_gbif <- function(.query, format = "SIMPLE_CSV"){
  # deal with user-specified taxonomic names
  if(!is.null(identify)){
    .query$filter <- rbind(
      .query$filter,
      data.frame(variable = "taxonKey",
                 logical = "==",
                 value = "`TAXON_PLACEHOLDER`",
                 query = ""))
  }
  result <- list(
    type = "data/occurrences",
    url = url_lookup("data/occurrences"),
    headers =  list(
      `User-Agent` = galah_version_string(), 
      `X-USER-AGENT` = galah_version_string(),
      `Content-Type` = "application/json",
      Accept = "application/json"),
    options = list(
      httpauth = 1,
      userpwd = paste0(
        pour("user", "username", .pkg = "galah"),
        ":",
        pour("user", "password", .pkg = "galah"))),
    body = build_predicates(.query$filter, 
                            .query$geolocate,
                            format = format))
  class(result) <- "query"
  return(result)
}

#' calculate the query to be returned for a given living atlas
#' @param .query An object of class `data_request()`
#' @noRd
#' @keywords Internal
collapse_occurrences_la <- function(.query){
  # set default columns
  if(is.null(.query$select)){
    .query$select <- galah_select(group = "basic")
  }
  # build a query
  query <- c(build_query(identify = .query$identify,
                         filter = .query$filter, 
                         location = .query$geolocate, 
                         data_profile = .query$data_profile),
             fields = "`SELECT_PLACEHOLDER`",
             qa = "`ASSERTIONS_PLACEHOLDER`",
             facet = "false", # not tested
             emailNotify = email_notify(),
             sourceTypeId = 2004,
             reasonTypeId = pour("user", "download_reason_id"),
             email = pour("user", "email"),
             dwcHeaders = "true")
  # DOI conditional on this service being offered
  if (!is.null(.query$mint_doi) & 
      pour("atlas", "region") == "Australia") {
    query$mintDoi <- .query$mint_doi
  }
  # build url
  url <- url_lookup("data/occurrences") |> 
    url_parse()
  url$query <- query
  # build output
  result <- list(
    type = "data/occurrences",
    url = url_build(url),
    headers = build_headers(),
    select = .query$select)
  class(result) <- "query"
  return(result)
}
