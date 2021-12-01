#' Choose filters to narrow down occurrence queries
#'
#' @param ... filters, in the form \code{field logical value}
#' @param profile \code{string}: (optional) a data quality profile to apply to the
#' records. See \code{\link{find_profiles}} for valid profiles. By default
#' no profile is applied.
#' @describeIn galah_filter Deprecated function name
#' @export

select_filters <- function(..., profile = NULL) {
  
  galah_filter(..., profile)
  
}