#' Internal function to `collapse()` for `type = "occurrences"`
#' @noRd
#' @keywords Internal
collapse_occurrences <- function(.data){
  if(is_gbif()){
    function_name <- "collapse_occurrences_gbif"
    .data$format <- "SIMPLE_CSV"
    arg_names <- names(formals(collapse_occurrences_gbif))
  }else{
    function_name <- "collapse_occurrences_atlas"
    arg_names <- names(formals(collapse_occurrences_atlas))
  }
  custom_call <- .data[names(.data) %in% arg_names]
  class(custom_call) <- "data_request"
  request <- do.call(function_name, custom_call)
  return(request)
}


#' calculate the query to be returned for a given living atlas
#' @noRd
#' @keywords Internal
collapse_occurrences_atlas <- function(identify = NULL,
                                       filter = NULL,
                                       geolocate = NULL,
                                       data_profile = NULL,
                                       select = NULL,
                                       slice = NULL,
                                       mint_doi = FALSE){
  # set default columns
  if(is.null(select)){
    select <- galah_select(group = "basic")
  }
  
  # build a query
  query <- c(build_query(identify,
                         filter = filter, 
                         location = geolocate, 
                         data_profile = data_profile$data_profile),
             fields = build_columns(select[select$type != "assertion", ]),
             qa = build_assertion_columns(select),
             facet = "false", # not tested
             emailNotify = email_notify(),
             sourceTypeId = 2004,
             reasonTypeId = pour("user", "download_reason_id"),
             email = pour("user", "email"),
             dwcHeaders = "true")
  
  # DOI conditional on this service being offered
  if (mint_doi & pour("atlas", "region") == "Australia") {
    query$mintDoi <- "true"
  }
  
  # handle slice
  if(!is.null(slice)){
    query$pageSize <- slice$slice_n
  }

  # build url
  url <- url_lookup("records_occurrences") |> url_parse()
  url$query <- query
  
  # build output
  result <- list(
    type = "occurrences",
    url = url_build(url),
    headers = build_headers())
  class(result) <- "data_query"
  
  return(result)
}

#' calculate the query to be returned for gbif
#' @noRd
#' @keywords Internal
collapse_occurrences_gbif <- function(identify = NULL,
                                      filter = NULL,
                                      geolocate = NULL,
                                      format = "SIMPLE_CSV",
                                      data_profile = NULL){
  # deal with user-specified taxonomic names
  if(!is.null(identify)){
    filter <- rbind(
      filter,
      data.frame(variable = "taxonKey",
                 logical = "==",
                 value = identify$identifier,
                 query = ""))
  }
  result <- list(
    type = "occurrences",
    url = url_lookup("records_occurrences"),
    headers =  list(
      `User-Agent` = galah_version_string(), # or "r-curl/4.3.3 crul/1.3 galah/1.5.1"
      `X-USER-AGENT` = galah_version_string(),
      `Content-Type` = "application/json",
      Accept = "application/json"),
    opts = list(
      httpauth = 1,
      userpwd = paste0(
        pour("user", "username", .pkg = "galah"),
        ":", 
        pour("user", "password", .pkg = "galah"))),
    body = build_predicates(filter, format))
  class(result) <- "data_query"
  return(result)
}