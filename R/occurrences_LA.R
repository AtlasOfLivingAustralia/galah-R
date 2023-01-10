# internal workhorse function
occurrences_LA <- function(identify = NULL,
                                       filter = NULL,
                                       geolocate = NULL,
                                       data_profile = NULL,
                                       select = NULL,
                                       mint_doi = FALSE,
                                       doi = NULL,
                                       refresh_cache = FALSE) {

  # check whether API exists
  occurrences_url <- url_lookup("records_occurrences")

  # more checks
  verbose <- getOption("galah_config")$package$verbose
  assert_that(is.logical(mint_doi))
  if(!is.null(doi)){
    abort("Argument `doi` is deprecated; use `collect_occurrences()` instead")
  }

  # If no filters are specified, reject
  if(
    all(unlist(lapply(list(identify, filter, geolocate, doi), is.null)))
  ){
    too_many_records(max_count = 5 * 10^7)
  }

  # set default columns
  if(is.null(select)){
    select <- galah_select(group = "basic")
  }

  # ensure profile works from galah_filter as well as galah_profile  
  if(is.null(data_profile)){
    if(is.null(filter)){
      profile <- NULL
    }else{
      profile <- extract_profile(filter)
    }
  }else{
    profile <- data_profile$data_profile
  }

  query <- build_query(identify, 
                       filter = filter, 
                       location = geolocate, 
                       profile = profile)
  
  # Check record count
  if (getOption("galah_config")$package$run_checks) {
    count <- record_count(query)
    if (is.null(count)){
      system_down_message("atlas_occurrences")
      return(tibble())
    }else{
      check_count(count, 5 * 10^7) # aborts under selected circumstances
    }
  }

  query <- c(query,
    fields = build_columns(select[select$type != "assertion", ]),
    qa = build_assertion_columns(select),
    emailNotify = email_notify(),
    sourceTypeId = 2004,
    reasonTypeId = getOption("galah_config")$user$download_reason_id,
    email = user_email(),
    dwcHeaders = "true")

  if (mint_doi & getOption("galah_config")$atlas$region == "Australia") {
    query$mintDoi <- "true"
  }

  status_initial <- url_GET(occurrences_url, params = query)

  # Get data  
  if(is.null(status_initial)){
    return(NULL)
  }
  download_resp <- url_queue(status_initial)
  if(is.null(download_resp)){
    inform("Calling the API failed for `atlas_occurrences`")
    return(tibble())
  }

  # download from url
  result <- url_download(download_resp, ext = "zip")
  if(is.null(result)){
    system_down_message("atlas_occurrences")
  }else{
    result
  }
}