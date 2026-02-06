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
    download_failed_message(call = call)
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
    cli::cli_abort("No response from selected atlas.",
                   call = call)
  }
  # get data
  if(potions::pour("package", "verbose", .pkg = "galah") &
     download_response$status == "complete") {
    
    scrolly_dots_message("Downloading")
    # cli::cli_par()
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
    download_failed_message(call = call)
  }else{
    result <- result |>
      check_field_identities(.query, error_call = call) |>
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
    download_failed_message(call = call)
  }else{
    result
  }
}

#' collect type `data/occurrences-glimpse`
#' @noRd
#' @keywords Internal
collect_occurrences_glimpse <- function(.query){
  result <- query_API(.query)

  # pull required info from API
  df_list <- result |>
    purrr::pluck("occurrences") |>
    # non-standard fields are nested within `otherProperties`
    # extract these
    purrr::map(\(a){
      if(any(names(a) == "otherProperties")){
        c(a[names(a) != "otherProperties"],
          a[["otherProperties"]])
      }
    }) 

  # create a tibble
  df <- dplyr::bind_rows(df_list) 
  attr(df, "total_n") <- result$totalRecords

  # assign new object for bespoke printing
  if(tibble::is_tibble(df)){
    structure(df, 
              class = c("occurrences_glimpse", "tbl_df", "tbl", "data.frame")) 
  }else{
    df # not sure what use case this is, but probably NULL
  }
}

#' Download failed message
#' @noRd
#' @keywords Internal
download_failed_message <- function(call){
  c("Download failed.",
    i = "This usually suggests a problem with the download itself, rather than the API.",
    i = "Consider checking that a file has been created in the expected location.") |>
    cli::cli_abort(call = call)
}



#' Theatrics
#' @noRd
#' @keywords Internal
scrolly_dots_message <- function(message) {
  
  spinny <- cli::make_spinner(
    which = "simpleDotsScrolling",
    template = paste0(message, " {spin}")
  )
  
  # update the spinner 100 times
  lapply(1:100, function(x) {
    spinny$spin()
    wait(.001)
  })
  
  # clear the spinner from the status bar
  # spinny$finish()
}

#' Wait time
#' @noRd
#' @keywords Internal
wait <- function(seconds = 1) {
  Sys.sleep(seconds)
}
