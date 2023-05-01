#' Collect data from the selected atlas
#'
#' In test
#' `r lifecycle::badge("experimental")` 
#' @seealso [atlas_occurrences()]
#' @param x An object of class `data_response`, created using 
#' [compute.data_request()]
#' @return A `tibble` containing requested data
#' @export
collect.data_response <- function(x, wait = FALSE){
  
  if(missing(x)){
    bullets <- c("`collect` requires an object of type `data_response`",
                 i = "Did your pipe start with `galah_call()` and end with `compute()`?")
    abort(bullets)
  }
  if(is.null(x) | !inherits(x, "data_response")){
    abort("Provided object does not contain any data")
  }
  
  ## This code from occurrences_LA
  if(wait){
    download_resp <- url_queue(x)
  }else{
    download_resp <- url_GET(x$statusUrl)
  }
  if(is.null(download_resp)){
    bullets <- c("`collect` did not return any information, despite receiving a valid input",
                 i = "Are you connected to the internet?")
    abort(bullets)
  }
  
  verbose <- getOption("galah_config")$package$verbose
  if(verbose){
    glue("Compute status: {download_resp$status}") |> 
    inform()
  }
  
  # download from url
  if(download_resp$status == "finished"){
    result <- url_download(download_resp$downloadUrl, ext = "zip")
    if(is.null(result)){
      system_down_message("collect")
    }else{
      result
    }    
  }

}