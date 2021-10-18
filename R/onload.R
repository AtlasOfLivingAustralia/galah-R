.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
      galah_config() ## will set to default values if not already set
      galah_config(
        valid_fields = sort(unique(c(search_fields()$id, all_fields()$name)))
      )
    }
}
