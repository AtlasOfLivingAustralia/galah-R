# internal workhorse function
atlas_occurrences_GBIF <- function(identify = NULL,
                                       filter = NULL,
                                       geolocate = NULL,
                                       refresh_cache = FALSE) {

  # check whether API exists
  occurrences_url <- url_lookup("records_occurrences")
  verbose <- getOption("galah_config")$package$verbose

  # set GBIF-specific problems
  if(!(nchar(getOption("galah_config")$user$username) > 0)){
    abort("GBIF requires a username to download occurrences")
  }
  if(!(nchar(getOption("galah_config")$user$password) > 0)){
    abort("GBIF requires a password to download occurrences")
  }

  browser()

  # If no filters are specified, reject
  if(
    all(unlist(lapply(list(identify, filter, geolocate), is.null)))
  ){
    too_many_records(max_count = 101000)
  }

  query <- build_query(identify, filter, geolocate)

  # Check record count
  if (getOption("galah_config")$package$run_checks) {
    count <- record_count(query)
    if (is.null(count)){
      system_down_message("atlas_occurrences")
      return(tibble())
    }else{
      check_count(count, max_count = 101000) # aborts under selected circumstances
    }
  }

  tmp <- tempfile()

  if(!is.null(identify)){
    filter <- rbind(
      filter,
      data.frame(variable = "taxonKey",
                  logical = "==",
                  value = identify$identifier,
                  query = ""))
  }
  headers <- list(
    `User-Agent` =  galah_version_string(), # or "r-curl/4.3.3 crul/1.3 galah/1.5.1"
    `X-USER-AGENT` =  galah_version_string(),
    `Content-Type` = "application/json",
    Accept = "application/json")
  opts <- list(
      httpauth = 1,
      userpwd = paste0(
        getOption("galah_config")$user$username,
        ":", 
        getOption("galah_config")$user$password))

  status_initial <- url_POST(occurrences_url,
                headers,
                opts,
                body = build_predicates(filter))

  # Get data  
  if(is.null(status_initial)){
    return(NULL)
  }
  download_resp <- url_queue(status_initial) # up to here
  if(is.null(download_resp)){
    inform("Calling the API failed for `atlas_occurrences`")
    return(tibble())
  }

  # download from url
    # check queue - this section assumes it has finished, but check_queue uses a loop to iterate
  result <- paste0("https://api.gbif.org/v1/occurrence/download/", download_resp) |>
    url_GET()

  # at this point url_queue() returns the link; i.e.
  # return(status$downloadLink)

  # download
  result <- url_download(status$downloadLink, ext = "zip")

  if(is.null(result)){
    system_down_message("atlas_occurrences")
  }else{
    result
  }
}