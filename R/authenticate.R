#' Set up authentication
#' 
#' Add an authentication slot to a query. That slot is then used by later
#' code to determine whether to add an OAuth workflow. It is triggered
#' automatically within [capture()] if the `authenticate` argument
#' of [galah_config()] is set to `TRUE`, but only for occurrence queries
#' to the Atlas of Living Australia.
#' `r lifecycle::badge("experimental")`.
#' @param .data An object of class `data_request` or `metadata_request`
#' @param cache_disk (logical) Should JWT tokens be cached to disk? Defaults
#' to `FALSE`
#' @returns An object of the same class as supplied, but with an added
#' `authenticate` slot.
#' @export
authenticate <- function(.data,
                         cache_disk = FALSE){
  update_request_object(.data,
                        authenticate = list(use_jwt = TRUE,
                                            use_apikey = FALSE, # not supported yet
                                            cache_disk = cache_disk))
}