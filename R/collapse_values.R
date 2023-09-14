#' Internal function to run `collapse()` for `request_values(type = "collections")`
#' @noRd
#' @keywords Internal
collapse_collection_values <- function(.data){
  result <- list(
    type = .data$type,
    url = paste(url_lookup("collections_collections"),
                .data$filter$selection[1],
                sep = "/"))
  class(result) <- "values_query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "datasets")`
#' @noRd
#' @keywords Internal
collapse_dataset_values <- function(.data){
  result <- list(
    type = .data$type,
    url = paste(url_lookup("collections_datasets"),
                .data$filter$selection[1],
                sep = "/"))
  class(result) <- "values_query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "fields")`
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @noRd
#' @keywords Internal
collapse_field_values <- function(.data){
  if(is_gbif()){
    url <- url_lookup("records_counts") |> 
      url_parse()
    url$query <- list(
      facet = .data$filter$selection[1], 
      limit = 0, 
      facetLimit = 10^4) # FIXME: integrate with `slice_head()`
  }else{
    url <- url_lookup("records_facets") |> 
      url_parse()
    url$query <- list(
      facets = .data$filter$selection[1], 
      facetLimit = 10^4)
  }
  result <- list(
    type = .data$type,
    url = url_build(url))
  class(result) <- "values_query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "lists")`
#' @noRd
#' @keywords Internal
collapse_list_values <- function(.data){
  result <- list(
    type = .data$type,
    url = url_lookup("lists_lookup", 
                     list_id = .data$filter$selection[1]))
  class(result) <- "values_query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "profiles")`
#' @noRd
#' @keywords Internal
collapse_profile_values <- function(.data){
  result <- list(
    type = .data$type,
    url = url_lookup("profiles_lookup", 
                     profile = .data$filter$selection[1]))
  class(result) <- "values_query"
  return(result)
}

#' Internal function to run `collapse()` for `request_values(type = "providers")`
#' @noRd
#' @keywords Internal
collapse_provider_values <- function(.data){
  result <- list(
    type = .data$type,
    url = paste(url_lookup("collections_providers"),
                .data$filter$selection[1],
                sep = "/"))
  class(result) <- "values_query"
  return(result)
}

#' Internal function to `collapse()` for `type = "taxonomy"`
#' @importFrom rlang abort
#' @importFrom utils URLencode
#' @noRd
#' @keywords Internal
collapse_taxa_values <- function(.data){
  id <- as.character(.data$filter$selection) |>
    URLencode(reserved = TRUE)
  result <- list(type = .data$type,
                 url = url_lookup("species_children", id = id),
                 headers = build_headers())
  class(result) <- "values_query"
  return(result)
}