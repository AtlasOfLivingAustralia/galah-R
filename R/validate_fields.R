# Validate fields passed as arguments in `galah`
# Internal use only

validate_fields <- function(named_field){

  load_fields()
  
  invalid_fields <- named_field[!is.element(named_field, galah_config()$valid_fields)]
  if (length(invalid_fields) > 0) {
    if(!all(invalid_fields %in% image_fields())){ # exception for ala_media
      message("The following fields may be invalid: ",
           paste(invalid_fields, collapse = ", "),
           ". Use `search_fields()` to get a list of valid options")
    }
  }
}


load_fields <- function(){
  if(length(galah_config()$valid_fields) < 1){
    galah_config(valid_fields = unique(c(search_fields()$id, all_fields()$name)))
  }
}