#' workhorse function to get occurrences from an atlas
#' @param .query an object of class `data_response`, created using 
#' `compute.data_request()`
#' @param wait logical; should we ping the API until successful? Defaults to 
#' FALSE
#' @param file character; optional name for the downloaded file. Defaults to 
#' `data` followed by the system time in `%Y-%m-%d_%H-%M-%S` format, with a 
#' `.zip` suffix.
#' @noRd
#' @keywords Internal
collect_occurrences <- function(.query, 
                                wait, 
                                file = NULL,
                                error_call = rlang::caller_env()){
  switch(potions::pour("atlas", "region"),
         "Austria" = collect_occurrences_direct(.query,
                                                file = file,
                                                call = error_call),
         "United Kingdom" = collect_occurrences_direct(.query,
                                                       file = file,
                                                       call = error_call),
         collect_occurrences_default(.query,
                                     wait = wait,
                                     file = file,
                                     call = error_call))
}

#' Internal function to `collect_occurrences()` for UK
#' @noRd
#' @keywords Internal
collect_occurrences_direct <- function(.query, file, call){
  .query$download <- TRUE
  .query$file <- check_download_filename(file)
  query_API(.query)
  result <- read_zip(.query$file)
  if(is.null(result)){
    cli::cli_inform("Download failed", call = call)
    return(tibble::tibble())
  }else{
    result
  }
}

#' Internal function to `collect_occurrences()` for living atlases
#' @noRd
#' @keywords Internal
collect_occurrences_default <- function(.query, wait, file, call){
  # check queue
  download_response <- check_queue(.query, wait = wait)
  if(is.null(download_response)){
    cli::cli_abort("No response from selected atlas",
                   call = call)
  }
  # get data
  if(potions::pour("package", "verbose", .pkg = "galah") &
     download_response$status == "complete") {
    cli::cli_inform("Downloading")
  }
  # sometimes lookup info critical, but not others - unclear when/why!
  if(any(names(download_response) == "download_url")){
    new_object <- list(type = "data/occurrences",
                       url = download_response$download_url,
                       download = TRUE,
                       file = check_download_filename(file)) |>
      as_query()
    # run downloads
    query_API(new_object)
    # import
    result <- read_zip(new_object$file)
  }else{
    return(download_response) 
  }
  # handle result
  if(is.null(result)){
    cli::cli_inform("Download failed", call = call)
    return(tibble::tibble())
  }else{
    result <- result |>
      check_field_identities(.query) |>
      check_media_cols()  # check for, and then clean, media info
    # exception for GBIF to ensure DOIs are preserved
    if(!is.null(download_response$doi)){
      # NOTE: GBIF documents DOIs in download response status url (it used to be automatically appended)
      #       We extract and preserve this info for the user, as of 2025-06-10
      doi <- download_response$doi
      attr(result, "doi") <- glue::glue("https://doi.org/{doi}")
    }
    if(!is.null(.query$search_url)){
      attr(result, "search_url") <- .query$search_url
    }
    result
  }
}

#' Subset of collect for a doi.
#' @param .query An object of class `data_request`
#' @noRd
#' @keywords Internal
collect_occurrences_doi <- function(.query, 
                                    file = NULL, 
                                    call) {
  .query$file <- check_download_filename(file)
  query_API(.query)
  result <- read_zip(.query$file)
  if(is.null(result)){
    cli::cli_inform("Download failed.", call = call)
    tibble::tibble()
  }else{
    result
  }
}