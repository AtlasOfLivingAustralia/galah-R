# Validate fields passed as arguments in `galah`
# Internal use only

validate_fields <- function(named_field){

  invalid_fields <- named_field[!is.element(named_field, show_all_fields()$id)]
  if (length(invalid_fields) > 0) {
    if(!all(invalid_fields %in% image_fields())){ # exception for ala_media
      message("The following fields may be invalid: ",
           paste(invalid_fields, collapse = ", "),
           ". Use `show_all_fields()` to get a `data.frame` of valid options")
    }
  }
}