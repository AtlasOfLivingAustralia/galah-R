#' Internal function to convert `data_request` with `type = "occurrences"` to a `query`
#' @noRd
#' @keywords Internal
capture_occurrences <- function(.query,
                                 ...,
                                 error_call = rlang::caller_env()){
  if(is.null(.query$filter) & 
     is.null(.query$identify) & 
     is.null(.query$geolocate)){
    cli::cli_abort("No filters supplied to `capture()` with `type = \"occurrences\"`",
                   call = error_call)
  }
  switch(potions::pour("atlas", "region"),
         "United Kingdom" = capture_occurrences_uk(.query, ...),
         "Global" = capture_occurrences_gbif(.query, ...),
         capture_occurrences_la(.query, ...))
}

#' calculate the query to be returned for the UK atlas
#' @param .query An object of class `data_request()`
#' @noRd
#' @keywords Internal
capture_occurrences_uk <- function(.query, ...){
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
                             apply_profile = .query$apply_profile),
                 fields = "`SELECT_PLACEHOLDER`",
                 qa = "`ASSERTIONS_PLACEHOLDER`",
                 sourceTypeId = source_type_id_lookup("United Kingdom"),
                 fileType = "csv",
                 reasonTypeId = potions::pour("user", "download_reason_id"),
                 dwcHeaders = "true")
  
  # build output
  list(type = "data/occurrences",
       url = httr2::url_build(url),
       headers = build_headers()) |>
    as_prequery()
}

#' calculate the query to be returned for GBIF
#' @noRd
#' @keywords Internal
capture_occurrences_gbif <- function(.query, 
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
    as_prequery()
}

#' calculate the query to be returned for a given living atlas
#' @param .query An object of class `data_request()`
#' @noRd
#' @keywords Internal
capture_occurrences_la <- function(.query,
                                    mint_doi = FALSE){
  # build a query
  query <- c(build_query(identify = .query$identify,
                         filter = .query$filter, 
                         location = .query$geolocate, 
                         apply_profile = .query$apply_profile),
             fields = "`SELECT_PLACEHOLDER`",
             qa = "`ASSERTIONS_PLACEHOLDER`",
             facet = "false",
             sourceTypeId = {potions::pour("atlas", "region") |>
                             source_type_id_lookup()},
             reasonTypeId = potions::pour("user", "download_reason_id"),
             dwcHeaders = "true") |>
    add_email_notify() |>
    add_email_address(query = .query) |>
    add_doi_request(query = .query)

  # build url
  url <- url_lookup("data/occurrences") |> 
    httr2::url_parse()
  url$query <- query
  
  # build output
  list(type = "data/occurrences",
       url = httr2::url_build(url),
       headers = build_headers()) |>
    as_prequery()
}

#' Internal function to convert `data_request` with `type = "doi"` to a `query`
#' @noRd
#' @keywords Internal
capture_occurrences_doi <- function(.query, 
                                    error_call = rlang::caller_env()){
  if(is.null(.query$filter)){
    cli::cli_abort("A DOI must be specified using `filter(doi == \"my-doi-here\")`.", 
                   call = error_call)
  }
  
  if(is.null(.query$filter$variable) && .query$filter$variable != "doi"){
    cli::cli_abort("No DOI has been supplied.", 
                   call = error_call)
  }
  
  atlas <- potions::pour("atlas", "acronym")
  if(!(atlas %in% c("ALA", "GBIF"))){
    c(
      "DOI downloads not supported by selected atlas.",
      i = "`request_data(type = \"occurrences-doi\")` has only been implemented for ALA & GBIF") |>
    cli::cli_abort(call = error_call)    
  }
  
  doi <- .query$filter$value[[1]]
  
  # remove "https://" if present
  if (grepl("^http://doi.org/", doi)) {
    doi <- sub("^https://doi.org/", "", doi)
  }
  
  # extract useful part of DOI
  doi_str <- stringr::str_split(doi, "ala.")[[1]][2]
  if(is.na(doi_str)){
    c(
      "DOI has not been generated by the ALA.",
      i = "DOIs created by the ALA have a prefix of 10.26197/ala.") |>
    cli::cli_abort(call = error_call)
  }
  
  list(type = "data/occurrences-doi",
       url = url_lookup("data/occurrences-doi", 
                        doi_string = doi_str),
       headers = build_headers(),
       download = TRUE) |>
  as_query()
}