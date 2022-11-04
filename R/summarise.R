#' Summarise for object of class `data_request`
#' @description `r lifecycle::badge("experimental")` 
#' @rdname atlas_counts
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @exportS3Method dplyr::summarise
#' @export
summarise.data_request <- function(.data, type = c("record", "species")){
  type <- match.arg(type)
  atlas_counts(
    request = .data,
    type = type)
}

#' Summarize for object of class `data_request`
#' @rdname atlas_counts
#' @exportS3Method dplyr::summarize
#' @export
summarize.data_request <- function(...){
  summarise.data_request(...)
}