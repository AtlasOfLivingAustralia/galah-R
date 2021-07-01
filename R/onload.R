.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
        ala_config() ## will set to default values if not already set
    }
}
