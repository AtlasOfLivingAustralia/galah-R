#' Join the queue to download records from the chosen atlas
#'
#' In test
#' `r lifecycle::badge("experimental")` 
#' @seealso [atlas_occurrences()]
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @return An object of class `request_info`
#' @importFrom potions pour
#' @importFrom rlang abort
#' @export
compute.data_request <- function(x){

  if(missing(x)){
    bullets <- c("`compute` requires an object of type `data_request`",
                 i = "Did you use `galah_call() to start your pipe?")
    abort(bullets)
  }
  
  # choose behavior depending on whether we are calling LAs or GBIF
  if(is_gbif()){
    function_name <- "occurrences_GBIF"
    x$format <- "SIMPLE_CSV"
    arg_names <- names(formals(occurrences_GBIF))
  }else{
    function_name <- "occurrences_LA"
    arg_names <- names(formals(occurrences_LA))
  }
  
  # subset to available arguments
  custom_call <- x[names(x) %in% arg_names]
  if(!is.null(custom_call$doi)){
    custom_call <- custom_call["doi"]
  }
  class(custom_call) <- "data_request"
  
  # check for caching
  cache_file <- cache_filename("occurrences", unlist(custom_call))
  if (pour("package", "caching") && 
      file.exists(cache_file) && !refresh_cache) {
    return(read_cache_file(cache_file))
  }else{
    return(do.call(function_name, custom_call))
  }

}