#' workhorse function to get occurrences from an atlas
#' @param .query an object of class `data_response`, created using 
#' `compute.data_request()`
#' @param wait logical; should we ping the API until successful? Defaults to 
#' FALSE
#' @param file character; optional name for the downloaded file. Defaults to 
#' `data` followed by the system time in `%Y-%m-%d_%H-%M-%S` format, with a 
#' `.zip` suffix.
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
collect_occurrences <- function(.query, wait, file = NULL){
  switch(pour("atlas", "region"),
         "Austria" = collect_occurrences_direct(.query, file = file),
         "United Kingdom" = collect_occurrences_direct(.query, file = file),
         collect_occurrences_default(.query, wait = wait, file = file))
}

#' Internal function to `collect_occurrences()` for UK
#' @noRd
#' @keywords Internal
collect_occurrences_direct <- function(.query, file){
  .query$download <- TRUE
  .query$file <- check_download_filename(file)
  query_API(.query)
  result <- read_zip(.query$file)
  if(is.null(result)){
    inform("Download failed")
    return(tibble())
  }else{
    result
  }
}

#' Internal function to `collect_occurrences()` for living atlases
#' @noRd
#' @keywords Internal
collect_occurrences_default <- function(.query, wait, file){
  # check queue
  download_response <- check_queue(.query, wait = wait)
  if(is.null(download_response)){
    abort("No response from selected atlas")
  }
  # get data
  if(pour("package", "verbose", .pkg = "galah") &
     download_response$status == "complete") {
    inform("Downloading")
  }
  # sometimes lookup info critical, but not others - unclear when/why!
  if(any(names(download_response) == "download_url")){
    new_object <- list(url = download_response$download_url,
                       download = TRUE)
    new_object$file <- check_download_filename(file)
    query_API(new_object)
    result <- read_zip(new_object$file)
  }else{
    return(download_response) 
  }
  # handle result
  if(is.null(result)){
    inform("Download failed")
    return(tibble())
  }else{
    result <- result |>
      check_field_identities(.query) |>
      check_media_cols()  # check for, and then clean, media info
    # exception for GBIF to ensure DOIs are preserved
    if(!is.null(.query$doi)){
      attr(result, "doi") <- paste0("https://doi.org/", .query$doi)
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
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
collect_occurrences_doi <- function(.query, 
                                    file = NULL, 
                                    error_call = caller_env()) {
  .query$file <- check_download_filename(file)
  query_API(.query)
  result <- read_zip(.query$file)
  if(is.null(result)){
    inform("Download failed.")
    tibble()
  }else{
    result
  }
}