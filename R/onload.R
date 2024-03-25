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
      # add a note to the user
      galah_version <-  "version unknown"
      suppressWarnings(
        try(galah_version <- utils::packageDescription("galah")[["Version"]],
            silent = TRUE)) ## get the galah version, if we can
      bullets <- c(
        glue("galah: version {galah_version}"),
        # i = "By default, {galah} queries the Atlas of Living Australia (ALA).",
        i = "A list of supported GBIF nodes can be found using `show_all(atlases)`.",
        i = "The default node is ALA (ala.org.au). To change nodes, call e.g. `galah_config(atlas = 'GBIF')`."
      )
      inform(bullets)
    }
}
