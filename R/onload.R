#' Set-up for galah during loading
#' @noRd
#' @keywords Internal
#' @importFrom cli col_magenta
#' @importFrom glue glue
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
        i = col_magenta('Default node set to ALA (ala.org.au).'),
        i = col_magenta('See all supported GBIF nodes with `show_all(atlases)`.'),
        i = col_magenta('To change nodes, use e.g. `galah_config(atlas = \"GBIF\")`.')
      )
      inform(bullets,
             class = c("packageStartupMessage",  # see ?packageStartupMessage (required by `check()`)
                       "simpleMessage", 
                       "message", 
                       "condition"))
    }
}
