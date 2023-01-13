.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
      galah_config()
      options(list(
        "galah_internal_cache" = galah_internal_archived))
    }
}
