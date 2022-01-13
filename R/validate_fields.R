# Validate fields passed as arguments in `galah`
# Internal use only

validate_fields <- function(named_field){

  invalid_fields <- named_field[!is.element(named_field, show_all_fields()$id)]
  if (length(invalid_fields) > 0) {
    if(!all(invalid_fields %in% image_fields())){ # exception for ala_media
      list_invalid_fields <- glue::glue_collapse(invalid_fields, 
                                                 sep = ", ")
      bullets <- c(
        glue("Invalid field(s) detected."),
        i = "See a listing of all valid fields with `show_all_fields()`.",
        i = "Search for the valid name of a desired field with `search_fields()`.",
        x = glue("Invalid field(s): {list_invalid_fields}."))
      warn(bullets)
    }
  }
}
