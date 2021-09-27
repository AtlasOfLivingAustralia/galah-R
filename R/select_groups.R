#' Specify groups for which counts are required
#' 
#' A function to specify how counts should be grouped using 
#' \code{\link{ala_counts}}.
#' 
#' @param ... one or more strings giving variable names (see \code{\link{search_fields}})
#' @param expand Logical: should all combinations of field levels be returned?
#' 
#' @details
#' If \code{expand} is \code{FALSE} (default) and >1 variables are specified
#' using \code{...}, then the counts for those variables are calculated separately.
#' If \code{TRUE}, then all n-wise combinations are calculated, where n is the
#' number of variables supplied.
#' 
#' @return An object of class \code{data.frame} and \code{ala_groups}
#' containing columns \code{name} (string) and \code{expand} (logical).
#' @export

select_groups <- function(..., expand = FALSE){
  result <- data.frame(
    name = c(...),
    expand = expand)
  class(result) <- c(class(result), "ala_groups")
  validate_facet(result$name)
  result
}