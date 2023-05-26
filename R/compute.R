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
#' [galah_call()], or of class `data_query`, created using 
#' `collapse.data_request()`
#' @param what string: what kind of data are requested?
#' @return An object of class `data_response`
#' @importFrom potions pour
#' @importFrom rlang abort
#' @export
compute.data_request <- function(.data, what){
  check_type(what)
  .data <- collapse(.data, what)
  switch_compute(.data, what)
}

#' @rdname compute.data_request
#' @export
compute.data_query <- function(.data){
  switch_compute(.data, what = .data$what)
}

#' Internal function to determine which type of call to compute
#' @noRd
#' @keywords Internal
switch_compute <- function(.data, what){
  if(what != "counts"){
    check_login(.data)
  }
  switch(what, 
         "counts" = compute_counts(.data),
         "species" = compute_species(.data),
         "occurrences" = compute_occurrences(.data),
         "media" = compute_media(.data))   
}