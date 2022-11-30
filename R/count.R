#' Count for object of class `data_request`
#' @description `r lifecycle::badge("experimental")` 
#' @param .data An object of class `data_request`, created using [galah_call()]
#' @param type `string`: one of `c("record", "species")`. Defaults to
#' "record". If "species", the number of species matching the criteria will be
#' returned, if "record", the number of records matching the criteria will be
#' returned.
#' @seealso  [atlas_counts()], with which this function is synonymous.
#' @importFrom dplyr count
#' @export
count.data_request <- function(.data, type = c("record", "species")){
  type <- match.arg(type)
  atlas_counts(
    request = .data,
    type = type)
}