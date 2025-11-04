#' Internal function to convert `data_request` with `type = "occurrences"` to a `query`
#' @noRd
#' @keywords Internal
as_query_occurrences <- function(.query,
                                 ...,
                                 error_call = rlang::caller_env()){
  if(is.null(.query$filter) & 
     is.null(.query$identify) & 
     is.null(.query$geolocate)){
    cli::cli_abort("No filters supplied to `collapse()` with `type = \"occurrences\"`",
                   call = error_call)
  }
  switch(potions::pour("atlas", "region"),
         "United Kingdom" = as_query_occurrences_uk(.query, ...),
         "Global" = as_query_occurrences_gbif(.query, ...),
         as_query_occurrences_la(.query, ...))
}

#' calculate the query to be returned for the UK atlas
#' @param .query An object of class `data_request()`
#' @noRd
#' @keywords Internal
as_query_occurrences_uk <- function(.query, ...){
  # set default columns
  if(is.null(.query$select)){
    .query$select <- galah_select(group = "basic")
  }
  # build a url
  # NOTE: providing an email blocks this from executing (2023-08-30)
  url <-  url_lookup("data/occurrences") |> 
    httr2::url_parse()
  url$query <- c(build_query(identify = .query$identify,
                             filter = .query$filter, 
                             location = .query$geolocate, 
                             data_profile = .query$data_profile),
                 fields = "`SELECT_PLACEHOLDER`",
                 qa = "`ASSERTIONS_PLACEHOLDER`",
                 sourceTypeId = source_type_id_lookup("United Kingdom"),
                 fileType = "csv",
                 reasonTypeId = potions::pour("user", "download_reason_id"),
                 dwcHeaders = "true")
  # build output
  list(type = "data/occurrences",
       url = httr2::url_build(url),
       headers = build_headers(),
       filter = .query$filter,
       select = .query$select) |>
    as_query()
}

#' calculate the query to be returned for GBIF
#' @noRd
#' @keywords Internal
as_query_occurrences_gbif <- function(.query, 
                                      format = "SIMPLE_CSV", 
                                      ...){
  # get user string
  username <- potions::pour("user", "username", .pkg = "galah")
  password <- potions::pour("user", "password", .pkg = "galah")
  user_string <- glue::glue("{username}:{password}")
  # build object
  list(type = "data/occurrences",
       url = url_lookup("data/occurrences"),
       headers =  list(
         `User-Agent` = galah_version_string(), 
         `X-USER-AGENT` = galah_version_string(),
         `Content-Type` = "application/json",
         Accept = "application/json"),
       options = list(
         httpauth = 1,
         userpwd = user_string),
       body = list(filter = .query$filter, 
                   identify = .query$identify,
                   geolocate = .query$geolocate,
                   format = "SIMPLE_CSV")) |>
    as_query()
}

#' calculate the query to be returned for a given living atlas
#' @param .query An object of class `data_request()`
#' @noRd
#' @keywords Internal
as_query_occurrences_la <- function(.query,
                                    mint_doi = FALSE){
  # set default columns
  if(is.null(.query$select)){
    .query <- .query |> select(group = "basic")
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
             sourceTypeId = {potions::pour("atlas", "region") |>
                             source_type_id_lookup()},
             reasonTypeId = potions::pour("user", "download_reason_id"),
             email = potions::pour("user", "email"),
             dwcHeaders = "true")
  # DOI conditional on this service being offered
  if(isTRUE(.query$mint_doi) & 
     potions::pour("atlas", "region") == "Australia"){
    query$mintDoi <- TRUE 
  }
  # build url
  url <- url_lookup("data/occurrences") |> 
    httr2::url_parse()
  url$query <- query
  # build output
  list(type = "data/occurrences",
       url = httr2::url_build(url),
       headers = build_headers(),
       filter = .query$filter,
       select = .query$select) |>
    as_query()
}
