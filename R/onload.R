.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
        ## populate the options slot
        ala_config() ## will set to default values if not already set
    }
}
