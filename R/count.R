#' @rdname atlas_counts
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param wt currently ignored
#' @param ... currently ignored
#' @param sort currently ignored
#' @param name currently ignored
#' @export
count.data_request <- function(x, ..., wt, sort, name, type = c("record", "species")){
  type <- match.arg(type)
  atlas_counts(
    request = x,
    type = type)
}