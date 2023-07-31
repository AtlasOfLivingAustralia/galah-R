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
  
  # .data <- check_occurrence_response(.data)
  # inform(glue("This query will return {.data$total_records} records."))
  
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
        if(pour("package", "verbose")){
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
  
  if(pour("package", "verbose")) {
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
      new_object$file <- paste0(pour("package", "directory"), "/data.zip")
      # check_path()? # currently commented out in check.R
    }
    query_API(new_object)
    result <- load_zip(new_object$file) # NOTE: this is *very* unlikely to work yet
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

check_occurrence_status <- function(.data){
  list(url = .data$status_url) |>
    query_API() |>
    as.list() |>
    check_occurrence_response()
}

#' Internal function to load zip files
#' @noRd
#' @keywords Internal
load_zip <- function(cache_dir){
  unzip(.cache_dir, exdir = cache_dir)
  all_files <- list.files(cache_dir)
  if(is_gbif()){
    import_files <- paste(cache_dir, 
                          all_files[grepl(".csv$", all_files)],
                          sep = "/")
    result <- read_tsv(import_files, col_types = cols()) |>
      suppressWarnings()
  }else{
    available_files <- all_files[grepl(".csv$", all_files) &
                                   grepl(paste0("^", data_prefix), all_files)]
    import_files <- paste(cache_dir, available_files, sep = "/")  
    result <- lapply(import_files, 
                     function(a){read_csv(a, col_types = cols()) |>
                         suppressWarnings()}) |> 
      bind_rows()
    
    # add doi when mint_doi = TRUE
    if(any(all_files == "doi.txt")){
      doi_file <- paste(cache_dir, "doi.txt", sep = "/")
      attr(result, "doi") <- read.table(doi_file) |> as.character()
    }
  }
  close(file(cache_dir))
  unlink(cache_dir)
  return(result)
}

#' Internal function to load a csv file downloaded by `query_API()`
#' MOVE THIS TO COLLECT_SPECIES()
#' @noRd
#' @keywords Internal
load_csv <- function(.data){
  tryCatch(
    read_csv(res$content, col_types = cols()),
    error = function(e) {
      e$message <- inform("No species matching the supplied filters were found.")
      close(file(cache_file))
      unlink(cache_file)
      stop(e)
    }
  )
}