#' Internal function to run `collapse()` for `request_values(type = "fields")`
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @noRd
#' @keywords Internal
collapse_fields_unnest <- function(.data){
  if(is_gbif()){
    url <- url_lookup("values/fields") |> 
      url_parse()
    url$query <- list(
      facet = .data$filter$value[1], 
      limit = 0, 
      facetLimit = 10^4) # FIXME: integrate with `slice_head()`
  }else{
    url <- url_lookup("metadata.fields-unnest") |> 
      url_parse()
    url$query <- list(
      facets = .data$filter$value[1], 
      facetLimit = 10^4)
  }
  result <- list(
    type = "metadata/fields-unnest",
    url = url_build(url))
  class(result) <- "query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "lists")`
#' @noRd
#' @keywords Internal
collapse_lists_unnest <- function(.data){
  result <- list(
    type = "metadata/lists-unnest",
    url = url_lookup("metadata/lists-unnest",
                     list_id = .data$filter$selection[1]))
  class(result) <- "query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "profiles")`
#' @noRd
#' @keywords Internal
collapse_profiles_unnest <- function(.data){
  result <- list(
    type = "metadata/profiles-unnest",
    url = url_lookup("metadata/profiles-unnest", 
                     profile = .data$filter$selection[1]))
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` for `type = "taxonomy"`
#' @importFrom rlang abort
#' @importFrom utils URLencode
#' @noRd
#' @keywords Internal
collapse_taxa_unnest <- function(.data){
  id <- as.character(.data$filter$selection) |>
    URLencode(reserved = TRUE)
  result <- list(type = .data$type,
                 url = url_lookup("metadata/taxa-unnest",
                                  id = id),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}