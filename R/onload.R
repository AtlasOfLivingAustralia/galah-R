#' Set-up for galah during loading
#' @noRd
#' @keywords Internal
#' @importFrom potions brew
.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
      brew(.pkg = "galah")
      galah_config() # to cache defaults
      options(list(
       "check_internal_cache" = galah_internal_cached))
    }
}
