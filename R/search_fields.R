#' Query layers, fields or assertions by free text search
#'
#' @param query `string`: A search string. Not case sensitive.
#' @param type `string`: What type of parameters should be searched?
#' Should be one or more of `fields`, `layers`, `assertions`,
#' `media` or `all`.
#' @return if `query` is missing, an empty `data.frame`; otherwise 
#' an object of class `tbl_df` and `data.frame` (aka a tibble) containing 
#' fields that match the search query.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Search for all fields that use include the word "date"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("date")
#' ```
#' 
#' Search for all fields with the string "basisofrecord"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("basisofrecord")
#' ```
#' 
#' Search for all fields that have information for "marine"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("marine") |> 
#'   head() # only show first 5 results
#' ```
#' 
#' Search for all Wordclim layers
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("worldclim", type = "layers")
#' ```
#' 
#' @rdname show_all_fields
#' @export search_fields

search_fields <- function(
  query,
  type = c("all", "fields", "layers", "assertions", "media", "other")
  ) {

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
    type <- match.arg(type)
    df <- show_all_fields(type = type)
    
    # merge information together into searchable strings
    df_string <- tolower(
      apply(df[, 1:2], 1, function(a){paste(a, collapse = " ")}))
      
    # return result of a grepl query
    df[grepl(tolower(query), df_string), ] |> 
      as_tibble()
  }
}
