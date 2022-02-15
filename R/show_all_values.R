#' @rdname show_all_minifunctions
#' @param field `string`: field to return the categories for
#' @export show_all_values

show_all_values <- function(field) {
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
 
  url <- server_config("records_base_url")
  resp <- atlas_GET(url, "occurrence/facets",
                    params = list(facets = field, flimit = 10^4))
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

#' @rdname search_minifunctions
#' @export search_values
search_values <- function(query, field){
  df <- show_all_values(field = field)
  df[grepl(tolower(query), tolower(df$category)), ]
}

# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}