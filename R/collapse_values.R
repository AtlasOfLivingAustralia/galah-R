#' Internal function to run `collapse()` for `request_values(type = "fields")`
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @noRd
#' @keywords Internal
collapse_field_values <- function(.data){
  if(is.null(.data$filter$selection)){
    bullets <- c("`collapse.values_request()` requires a `filter()` argument",
                 i = "e.g. `request_values() |> filter(field == basisOfRecord) |> collapse()`")
    abort(bullets)
  }
  if(is_gbif()){
    url <- url_lookup("records_counts") |> 
      url_parse()
    url$query <- list(
      facet = .data$filter$selection, 
      limit = 0, 
      facetLimit = 10^4) # FIXME: integrate with `slice_head()`
  }else{
    url <- url_lookup("records_facets") |> 
      url_parse()
    url$query <- list(
      facets = .data$filter$selection, 
      facetLimit = 10^4)
  }
  result <- list(
    type = "fields",
    url = url_build(url))
  class(result) <- "values_query"
  return(result)
}