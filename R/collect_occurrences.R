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
collect_occurrences <- function(.data, wait, file){
  
  # process supplied object
  if(.data$status == "incomplete"){
    if(wait){
      # THIS WON'T WORK YET!
      download_response <- do.call(lookup$queue_function, 
                                   lookup$queue_input)
    }else{
      # NOTE: this query does not appear to require an api key
      # if it does, then `compute_occurrences()` will require amendment to supply one
      download_response <- check_occurrence_status(.data)
      
      if(download_response$status == "incomplete"){
        if(pour("package", "verbose", .pkg = "galah")){
          inform("Your download isn't ready yet, please try again later!")
        }
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
  
  if(pour("package", "verbose", .pkg = "galah")) {
    inform(glue("
                
                Downloading
                
                "))
  }
  
  # get data
  # sometimes lookup info critical, but not others - unclear when/why!
  if(any(names(download_response) == "download_url")){
    new_object <- list(url = download_response$download_url,
                       download = TRUE)
    if(!missing(file)){
      new_object$file <- file
    }else{
      cache_directory <- pour("package", "directory", .pkg = "galah")
      current_time <- Sys.time() |> format("%Y-%m-%d_%H-%M-%S")
      new_object$file <- glue("{cache_directory}/data_{current_time}.zip") |>
        as.character()
      # check_path()? # currently commented out in check.R
    }
    query_API(new_object)
    result <- load_zip(new_object$file)
  }else{
    return(download_response) 
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
check_occurrence_response <- function(.data){
  if(is_gbif()){
    # list(
    #   completed_flag = "SUCCEEDED",
    #   queue_function = "check_queue_GBIF",
    #   queue_input = list(url = attr(.data, "url")),
    #   download_tag = "downloadLink",
    #   status_url = attr(.data, "url")
    # )
  }else{
    names(.data) <- camel_to_snake_case(names(.data))
    if(.data$status == "finished"){
      .data$status <- "complete"
    }else{
      .data$status <- "incomplete"
    }
    .data
  }
}

#' Internal function to change API response to contain standard headers
#' @noRd
#' @keywords Internal
check_occurrence_status <- function(.data){
  list(url = .data$status_url) |>
    query_API() |>
    as.list() |>
    check_occurrence_response()
}

#' Internal function to load zip files, without unzipping them first
#' @noRd
#' @keywords Internal
load_zip <- function(cache_file){
  # get names of files stored in .zip
  all_files <- unzip(cache_file, list = TRUE)$Name
  # zip files contain a lot of metadata that `galah` does not import
  # import only those files that meet our criteria for 'data'
  if(is_gbif()){
    available_files <- all_files[grepl(".csv$", all_files)]
    result <- unz(description = cache_file,  # require lapply?
                  filename = available_files) |> 
      read_tsv(col_types = cols()) |>
      suppressWarnings()
  }else{
    available_files <- all_files[grepl(".csv$", all_files) &
                                   grepl("^data|records", all_files)]
    result <- lapply(available_files, 
                     function(a, x){
                       # create connection to a specific file within zip
                       conn <- unz(description = x, 
                                   filename = a, 
                                   open = "rb")
                       # read
                       out <- read_csv(conn, 
                                       col_types = cols()) |>
                         suppressWarnings()
                       close(conn) # close connection
                       return(out)
                     }, x = cache_file) |>
      bind_rows()
    # # add doi when mint_doi = TRUE
    ## This needs to be re-enabled with new architecture
    # if(any(all_files == "doi.txt")){
    #   doi_file <- paste(cache_dir, "doi.txt", sep = "/")
    #   attr(result, "doi") <- read.table(doi_file) |> as.character()
    # }
  }
  return(result)
}