#' Join the queue to download records from the chosen atlas
#'
#' There are two versions of this function. If `compute` is called at the end of
#' a pipe started with `galah_call()`, then `compute.data_request()` is used.
#' If the user instead ends that chain with `collapse()`, then calling `compute`
#' generates a call to `compute.data_query()`. Either way the result will be 
#' identical.
#' `r lifecycle::badge("experimental")` 
#' @seealso [atlas_occurrences()]
#' @param .data An object of class `data_request` created using 
#' [galah_call()], or of class `data_query`, created using `collapse.data_request()`
#' @return An object of class `data_response`
#' @importFrom potions pour
#' @importFrom rlang abort
#' @export
compute.data_request <- function(.data,
                                 type = c("counts", "species", "occurrences", "media")){
  .data <- collapse(.data)
  type <- match.arg(type)
  switch_compute(.data, type)
}

#' @rdname compute.data_request
#' @export
compute.data_query <- function(.data){
  switch_compute(.data, type = .data$type)
}

#' Internal function to determine which type of call to compute
#' @noRd
#' @keywords Internal
switch_compute <- function(.data, type){
  check_login(.data)
  switch(type, 
         "counts" = compute_counts(.data),
         "species" = compute_species(.data),
         "occurrences" = compute_occurrences(.data),
         "media" = compute_media(.data))   
}

#' Workhorse function to compute occurrences
compute_occurrences <- function(.data){
  if(is_gbif()){
    status_code <- url_POST(.data$url,
                            headers = .data$headers,
                            opts = .data$opts,
                            body = .data$body)
  }else{
    status_code <- url_GET(.data$url, 
                           params = .data$query)
  }
  class(status_code) <- "data_response"
  return(status_code)
}

#' Internal function to confirm requisite login information has been provided
#' @noRd
#' @keywords Internal
#' @importFrom rlang caller_env
check_login <- function(.data, error_call = caller_env()){
  if(is_gbif()){
    if(.data$opts$userpwd == ":"){
      abort("GBIF requires a username and password to download occurrences or species")
    }
  }else{
    if(.data$query$email == ""){
      bullets <- c(
        "No user email was found.",
        i = glue("To download occurrence records you must provide a valid email ",
                 "address registered with the selected atlas using `galah_config(email = )`")
      )
      abort(bullets, call = error_call)
    }
  }
}