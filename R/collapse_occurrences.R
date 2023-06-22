collapse_occurrences <- function(.data){
  
  # choose behavior depending on whether we are calling LAs or GBIF
  if(is_gbif()){
    function_name <- "collapse_occurrences_gbif"
    .data$format <- "SIMPLE_CSV"
    arg_names <- names(formals(collapse_occurrences_gbif))
  }else{
    function_name <- "collapse_occurrences_atlas"
    arg_names <- names(formals(collapse_occurrences_atlas))
  }
  
  # subset to available arguments
  custom_call <- .data[names(.data) %in% arg_names]
  if(!is.null(custom_call$doi)){
    custom_call <- custom_call["doi"]
  }
  
  class(custom_call) <- "data_request"
  
  request <- do.call(function_name, custom_call)
  # request$count_query <- c(custom_call |> collapse_counts()) # add info to collect count
  
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
                                       mint_doi = FALSE){
  
  # check whether API exists
  base_url <- url_lookup("records_occurrences")
  
  # set default columns
  if(is.null(select)){
    select <- galah_select(group = "basic")
  }
  
  # build a query
  query <- c(build_query(identify, 
                         filter = filter, 
                         location = geolocate, 
                         profile = data_profile$data_profile),
             fields = build_columns(select[select$type != "assertion", ]),
             qa = build_assertion_columns(select),
             emailNotify = email_notify(),
             sourceTypeId = 2004,
             reasonTypeId = pour("user", "download_reason_id"),
             email = pour("user", "email"),
             dwcHeaders = "true")
  
  query <- build_fq(query) # messy but functional
  
  # DOI conditional on this service being offered
  if (mint_doi & pour("atlas", "region") == "Australia") {
    query$mintDoi <- "true"
  }
  
  result <- list(
    url = base_url,
    headers = list("User-Agent" = galah_version_string()),
    query = query)
  result$type <- "occurrences"
  class(result) <- "data_query"
  
  return(result)
}

# previously in utilities internal
build_fq <- function(params = list()) {
  # url <- parse_url(url)
  if(any(names(params) == "fq")){
    # join_char <- ifelse(length(url$query) > 0, "&fq=", "?fq=")
    
    # ensure all arguments from galah_filter are enclosed in brackets
    fq <- params$fq
    missing_brackets <- !grepl("^\\(", fq)
    if(any(missing_brackets)){
      fq[missing_brackets] <- paste0("(", fq[missing_brackets], ")")
    }
    fq_single <- paste(fq, collapse = "AND")
    return(c(fq = fq_single, params[names(params) != "fq"]))
    # build_url(url)
  }else{
    # build_url(url)
    return(params)
  }
}

#' calculate the query to be returned for gbif
#' @noRd
#' @keywords Internal
collapse_occurrences_gbif <- function(identify = NULL,
                                      filter = NULL,
                                      geolocate = NULL,
                                      format = "SIMPLE_CSV",
                                      data_profile = NULL){
  
  # check whether API exists
  occurrences_url <- url_lookup("records_occurrences")
  
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
    url = occurrences_url,
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
  result$type <- "occurrences"
  class(result) <- "data_query"
  
  return(result)
}