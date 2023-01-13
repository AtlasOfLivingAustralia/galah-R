# internal workhorse function
occurrences_GBIF <- function(identify = NULL,
                             filter = NULL,
                             geolocate = NULL,
                             format = "SIMPLE_CSV",
                             data_profile = NULL,
                             refresh_cache = FALSE) {
  
  # check whether API exists
  occurrences_url <- url_lookup("records_occurrences")
  verbose <- getOption("galah_config")$package$verbose

  # set GBIF-specific problems
  if(!(nchar(getOption("galah_config")$user$username) > 0)){
    abort("GBIF requires a username to download occurrences or species")
  }
  if(!(nchar(getOption("galah_config")$user$password) > 0)){
    abort("GBIF requires a password to download occurrences or species")
  }

  # If no filters are specified, reject
  if(
    all(unlist(lapply(list(identify, filter, geolocate), is.null)))
  ){
    too_many_records(max_count = 101000)
  }

  # Check record count
  if (getOption("galah_config")$package$run_checks && format == "SIMPLE_CSV") {
    query <- build_query(identify, filter, geolocate)
    count <- record_count(query)
    if (is.null(count)){
      system_down_message("atlas_occurrences")
      return(tibble())
    }else{
      check_count(count, max_count = 101000) # aborts under selected circumstances
    }
  }

  if(!is.null(identify)){
    filter <- rbind(
      filter,
      data.frame(variable = "taxonKey",
                  logical = "==",
                  value = identify$identifier,
                  query = ""))
  }
  headers <- list(
    `User-Agent` = galah_version_string(), # or "r-curl/4.3.3 crul/1.3 galah/1.5.1"
    `X-USER-AGENT` = galah_version_string(),
    `Content-Type` = "application/json",
    Accept = "application/json")
  opts <- list(
      httpauth = 1,
      userpwd = paste0(
        getOption("galah_config")$user$username,
        ":", 
        getOption("galah_config")$user$password))

  status_code <- url_POST(occurrences_url,
                          headers = headers,
                          opts = opts,
                          body = build_predicates(filter, format))

  # Get data  
  if(is.null(status_code)){
    return(NULL)
  }
  
  # Check queue until complete, with increasing time lags
  download_link <- paste0("https://api.gbif.org/v1/occurrence/download/", 
                          status_code) |>
    check_queue_GBIF()

  # download
  result <- url_download(download_link, ext = "zip")

  if(is.null(result)){
    system_down_message("atlas_occurrences")
  }else{
    attr(result, "doi") <- attr(download_link, "doi")
    result
  }
}