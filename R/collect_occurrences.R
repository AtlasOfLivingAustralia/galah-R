#' workhorse function to get occurrences from an atlas
#' @noRd
#' @param .data an object of class `data_response`, created using 
#' `compute.data_request()`
#' @param wait logical; should we ping the API until successful? Defaults to 
#' FALSE
#' @keywords Internal
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @importFrom tibble tibble
collect_occurrences <- function(.data, wait){
  
  # create a lookup table to ensure correct elements are used to LA/GBIF
  lookup <- occurrence_flags(.data)
  # inform(glue("This query will return {.data$query_n} records."))
  
  # process supplied object
  if(.data$status != lookup$completed_flag){
    if(wait){
      download_response <- do.call(lookup$queue_function, 
                                   lookup$queue_input)
    }else{
      download_response <- url_GET(lookup$status_url)
      if(download_response$status != lookup$completed_flag){
        class(download_response) <- "data_response"
        return(download_response)
      }
    }
  }else{
    download_response <- .data
  }
  
  if(is.null(download_response)){
    abort("No response from selected atlas")
  }
  
  if(pour("package", "verbose")) {
    inform(glue("
                
                Downloading
                
                "))
  }
  
  # get data
  # sometimes lookup info critical, but not others - unclear when/why!
  if(any(names(download_response) == lookup$download_tag)){
    result <- url_download(download_response[[lookup$download_tag]]) 
  }else{
    result <- url_download(download_response)
  }
  
  if(is.null(result)){
    inform("Download failed")
    return(tibble())
  }
  
  # rename cols so they match requested cols
  names(result) <- rename_columns(names(result), type = "occurrence")
  
  # replace 'true' and 'false' with boolean
  valid_assertions <- show_all_assertions()$id
  assertions_check <- names(result) %in% valid_assertions
  if(any(assertions_check)){
    result <- fix_assertion_cols(result, names(result)[assertions_check])
  }
  
  # check for, and then clean, media info
  result <- check_media_cols(result)
  return(result)
}

#' Internal function to ensure correct data extracted from API for LA/GBIF
#' @noRd
#' @keywords Internal
occurrence_flags <- function(.data){
  if(is_gbif()){
    list(
      completed_flag = "SUCCEEDED",
      queue_function = "check_queue_GBIF",
      queue_input = list(url = attr(.data, "url")),
      download_tag = "downloadLink",
      status_url = attr(.data, "url")
    )
  }else{
    list(
      completed_flag = "finished",
      queue_function = "url_queue",
      queue_input = list(status_initial = .data),
      download_tag = "downloadUrl",
      status_url = .data$statusUrl)
  }
}