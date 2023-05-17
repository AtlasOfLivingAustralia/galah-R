#' Collect data from the selected atlas
#'
#' In test
#' `r lifecycle::badge("experimental")` 
#' @seealso [atlas_occurrences()]
#' @param .data An object of class `data_response`, created using 
#' [compute.data_request()]
#' @return A `tibble` containing requested data
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @importFrom rlang inform
#' @export
collect.data_response <- function(.data, wait = FALSE){
  
  if(missing(.data)){
    abort(".data is missing, with no default")
  }else if(!inherits(.data, "data_response")){
    abort("`.data` must be of type `'data_response'`")
  }
  
  # needs to be some content in here to actually ping a web service!!
  # i.e. code broken here
  browser()
  # example pseduocode
  if(wait){
    url_queue()
  }else{
    url_GET()
  }
  
  # once complete, download from url
  if(download_resp$status == "finished"){ # not provided when wait = FALSE
    result <- url_download(download_resp$downloadUrl, ext = "zip")
    if(is.null(result)){
      system_down_message("collect")
    }else{
      result
    }    
  }

}