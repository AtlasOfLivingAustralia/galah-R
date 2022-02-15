#' @param field `string`: field to return the categories for. Use
#' [search_fields()] to view valid fields.
#' @param query `string`: A search string. Not case sensitive.
#' @rdname search_minifunctions
#' @export

search_values <- function(field, query){

  if (missing(field) || is.null(field)) {
    bullets <- c(
      "We didn't detect a field to search for.",
      i = "Try entering text to search within a given field.",
      i = "To see all valid fields, use `show_all_fields()`."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  }
  
   if (missing(query) || is.null(query)) {
     bullets <- c(
       "We didn't detect a valid query.",
       i = "Try entering text to search for matching values."
     )
     rlang::warn(message = bullets, error = rlang::caller_env())
   }
   
   field_text <- show_all_values(field)
   field_text[grepl(query, tolower(field_text$category)), ]
}