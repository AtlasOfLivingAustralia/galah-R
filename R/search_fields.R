#' Query layers, fields or assertions by free text search
#'
#' @param query `string`: A search string. Not case sensitive.
#' @param type `string`: What type of parameters should be searched?
#' Should be one or more of `fields`, `layers`, `assertions`,
#' `media` or `all`.
#' @return if `query` is missing, an empty `data.frame`; otherwise 
#' a `data.frame` containing fields that match the search query.
#' @rdname show_all_fields
#' @export search_fields

search_fields <- function(
  query,
  type = c("all", "fields", "layers", "assertions", "media", "other")
){

  if (missing(query) || is.null(query)) {
    as.data.frame(
      matrix(nrow = 0, ncol = 4, 
        dimnames = list(NULL, c("id", "description", "type", "link")))
    )
  }else{
    type <- match.arg(type)
    df <- show_all_fields(type = type)
    
    # merge info together into searchable strings
    df_string <- tolower(
      apply(df[, 1:2], 1, function(a){paste(a, collapse = " ")}))
      
    # return result of a grepl query
    df[grepl(tolower(query), df_string), ]
  }
}
