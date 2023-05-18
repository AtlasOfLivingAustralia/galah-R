#' function to check whether `type` arg is supplied to `collapse()` or `compute()`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_type <- function(type){
  if(missing(type)){
    bullets <- c("Argument `type` is missing, with no default",
                 i = "`type` must be one of 'counts', 'species', 'occurrences' or 'media'")
    abort(bullets)
  }
}