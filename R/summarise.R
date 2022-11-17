#' Summarise for object of class `data_request`
#' @description `r lifecycle::badge("experimental")` 
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param type `string`: one of `c("record", "species")`. Defaults to
#' "record". If "species", the number of species matching the criteria will be
#' returned, if "record", the number of records matching the criteria will be
#' returned.
#' @importFrom dplyr summarise
#' @export
summarise.data_request <- function(.data, type = c("record", "species")){
  type <- match.arg(type)
  atlas_counts(
    request = .data,
    type = type)
}

#' Summarize for object of class `data_request`
#' @rdname summarise.data_request
#' @importFrom dplyr summarize
#' @export
summarize.data_request <- function(.data, type = c("record", "species")){
  type <- match.arg(type)
  atlas_counts(
    request = .data,
    type = type)
}