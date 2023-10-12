#' Internal function to `collapse()` for `type = "occurrences"`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_occurrences <- function(.data, mint_doi = FALSE){
  if(is.null(.data$filter) & is.null(.data$identify)){
    abort("No filters supplied to atlas_occurrences()")
  }
  switch(pour("atlas", "region"),
         "United Kingdom" = collapse_occurrences_uk(.data),
         "Global" = collapse_occurrences_gbif(.data),
         {.data$mint_doi <- mint_doi
          collapse_occurrences_la(.data)})
}

#' calculate the query to be returned for the UK atlas
#' @param .data An object of class `data_request()`
#' @noRd
#' @keywords Internal
collapse_occurrences_uk <- function(.data){
  # set default columns
  if(is.null(.data$select)){
    .data$select <- galah_select(group = "basic")
  }
  # build a url
  # NOTE: providing an email blocks this from executing (2023-08-30)
  url <-  url_lookup("data/occurrences") |> 
    url_parse()
  url$query <- c(build_query(identify = .data$identify,
                             filter = .data$filter, 
                             location = .data$geolocate, 
                             data_profile = .data$data_profile$data_profile),
                 fields = build_columns(.data$select[.data$select$type != "assertion", ]),
                 qa = build_assertion_columns(.data$select),
                 sourceTypeId = 2001,
                 fileType = "csv",
                 reasonTypeId = pour("user", "download_reason_id"),
                 dwcHeaders = "true")
  # build output
  result <- list(
    type = "data/occurrences",
    url = url_build(url),
    headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' calculate the query to be returned for GBIF
#' @noRd
#' @keywords Internal
collapse_occurrences_gbif <- function(.data, format = "SIMPLE_CSV"){
  # deal with user-specified taxonomic names
  if(!is.null(identify)){
    .data$filter <- rbind(
      .data$filter,
      data.frame(variable = "taxonKey",
                 logical = "==",
                 value = "`TAXON_PLACEHOLDER`",
                 query = ""))
  }
  result <- list(
    type = "data/occurrences",
    url = url_lookup("data/occurrences"),
    headers =  list(
      `User-Agent` = galah_version_string(), # or "r-curl/4.3.3 crul/1.3 galah/1.5.1"
      `X-USER-AGENT` = galah_version_string(),
      `Content-Type` = "application/json",
      Accept = "application/json"),
    options = list(
      httpauth = 1,
      userpwd = paste0(
        pour("user", "username", .pkg = "galah"),
        ":", 
        pour("user", "password", .pkg = "galah"))),
    body = build_predicates(.data$filter, format = format))
  class(result) <- "query"
  return(result)
}

#' calculate the query to be returned for a given living atlas
#' @param .data An object of class `data_request()`
#' @noRd
#' @keywords Internal
collapse_occurrences_la <- function(.data){
  # set default columns
  if(is.null(.data$select)){
    .data$select <- galah_select(group = "basic")
  }
  # build a query
  query <- c(build_query(identify = .data$identify,
                         filter = .data$filter, 
                         location = .data$geolocate, 
                         data_profile = .data$data_profile$data_profile),
             fields = build_columns(.data$select[.data$select$type != "assertion", ]),
             qa = build_assertion_columns(.data$select),
             facet = "false", # not tested
             emailNotify = email_notify(),
             sourceTypeId = 2004,
             reasonTypeId = pour("user", "download_reason_id"),
             email = pour("user", "email"),
             dwcHeaders = "true")
  # DOI conditional on this service being offered
  if (.data$mint_doi & pour("atlas", "region") == "Australia") {
    query$mintDoi <- "true"
  }
  # build url
  url <- url_lookup("data/occurrences") |> 
    url_parse()
  url$query <- query
  # build output
  result <- list(
    type = "data/occurrences",
    url = url_build(url),
    headers = build_headers())
  class(result) <- "query"
  return(result)
}