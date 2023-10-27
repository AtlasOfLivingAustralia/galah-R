#' Internal function to `collapse()` for `type = "occurrences"`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_occurrences <- function(q_obj, mint_doi = FALSE){
  if(is.null(q_obj$filter) & is.null(q_obj$identify)){
    abort("No filters supplied to atlas_occurrences()")
  }
  switch(pour("atlas", "region"),
         "United Kingdom" = collapse_occurrences_uk(q_obj),
         "Global" = collapse_occurrences_gbif(q_obj),
         {q_obj$mint_doi <- mint_doi
          collapse_occurrences_la(q_obj)})
}

#' calculate the query to be returned for the UK atlas
#' @param q_obj An object of class `data_request()`
#' @noRd
#' @keywords Internal
collapse_occurrences_uk <- function(q_obj){
  # set default columns
  if(is.null(q_obj$select)){
    q_obj$select <- galah_select(group = "basic")
  }
  # build a url
  # NOTE: providing an email blocks this from executing (2023-08-30)
  url <-  url_lookup("data/occurrences") |> 
    url_parse()
  url$query <- c(build_query(identify = q_obj$identify,
                             filter = q_obj$filter, 
                             location = q_obj$geolocate, 
                             data_profile = q_obj$data_profile$data_profile),
                 fields = build_columns(q_obj$select[q_obj$select$type != "assertion", ]),
                 qa = build_assertion_columns(q_obj$select),
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
collapse_occurrences_gbif <- function(q_obj, format = "SIMPLE_CSV"){
  # deal with user-specified taxonomic names
  if(!is.null(identify)){
    q_obj$filter <- rbind(
      q_obj$filter,
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
    body = build_predicates(q_obj$filter, format = format))
  class(result) <- "query"
  return(result)
}

#' calculate the query to be returned for a given living atlas
#' @param q_obj An object of class `data_request()`
#' @noRd
#' @keywords Internal
collapse_occurrences_la <- function(q_obj){
  # set default columns
  if(is.null(q_obj$select)){
    q_obj$select <- galah_select(group = "basic")
  }
  # build a query
  query <- c(build_query(identify = q_obj$identify,
                         filter = q_obj$filter, 
                         location = q_obj$geolocate, 
                         data_profile = q_obj$data_profile$data_profile),
             # fields = build_columns(q_obj$select[q_obj$select$type != "assertion", ]),
             fields = "`SELECT_PLACEHOLDER`",
             qa = build_assertion_columns(q_obj$select),
             facet = "false", # not tested
             emailNotify = email_notify(),
             sourceTypeId = 2004,
             reasonTypeId = pour("user", "download_reason_id"),
             email = pour("user", "email"),
             dwcHeaders = "true")
  # DOI conditional on this service being offered
  if (q_obj$mint_doi & pour("atlas", "region") == "Australia") {
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
    headers = build_headers(),
    select = q_obj$select)
  class(result) <- "query"
  return(result)
}
