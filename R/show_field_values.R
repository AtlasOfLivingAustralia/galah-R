#' @rdname show_values
#' @param field `string`: field to return the categories for
#' @export show_field_values

show_field_values <- function(field) {
  if (missing(field)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a field to show values for?"
    )
    abort(bullets, call = caller_env())
  }

  if (!(field %in% show_all_fields()$id)) {
    bullets <- c(
      "Invalid field detected.",
      i = "Search for the valid name of a desired field with `search_fields()`.",
      x = glue("\"{field}\" is not a valid field.")
    )
    abort(bullets, call = caller_env())
  }
 
  resp <- atlas_url("records_facets") |>
          atlas_GET(params = list(facets = field, flimit = 10^4))
  if(is.null(resp)){
    bullets <- c(
      "Calling the API failed for `show_all_values()`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct."
      )
    inform(bullets)
    return(tibble())
  }else{
    category <- vapply(resp$fieldResult[[1]]$fq, function(n) {
      extract_category_value(n)
    }, USE.NAMES = FALSE, FUN.VALUE = character(1))
    cbind(field = field, as.data.frame(category)) |> as_tibble()
  }
}

# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}

#' @param field `string`: field to return the categories for. Use
#' [search_fields()] to view valid fields.
#' @param query `string`: A search string. Not case sensitive.
#' @rdname show_values
#' @export search_field_values

search_field_values <- function(field, query){
  
   if (missing(query) || is.null(query)) {
     bullets <- c(
       "We didn't detect a valid query.",
       i = "Try entering text to search for matching values."
     )
     rlang::warn(message = bullets, error = rlang::caller_env())
   }
   
   field_text <- show_field_values(field)
   field_text[grepl(query, tolower(field_text$category)), ]
}