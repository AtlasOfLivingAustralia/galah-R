#' @rdname search_minifunctions
#' @export search_fields

search_fields <- function(query){

  if (missing(query) || is.null(query)) {
    as.data.frame(
      matrix(nrow = 0, ncol = 4, 
        dimnames = list(NULL, c("id", "description", "type", "link")))
    )
    bullets <- c(
      "We didn't detect a field to search for.",
      i = "Try entering text to search for matching fields.",
      i = "To see all valid fields, use `show_all_fields()`."
    )
    rlang::warn(message = bullets, error = rlang::caller_env())
  } else {
    df <- show_all_fields()
    
    # merge information together into searchable strings
    df_string <- tolower(
      apply(df[, 1:2], 1, function(a){paste(a, collapse = " ")}))
      
    # return result of a grepl query
    df[grepl(tolower(query), df_string), ] |> 
      as_tibble()
  }
}
