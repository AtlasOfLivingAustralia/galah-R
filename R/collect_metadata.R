#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_apis <- function(.query){
  result <- .query$data |>
    parse(text = _) |> 
    eval()
  attr(result, "call") <- "apis"
  attr(result, "region") <- potions::pour("atlas", "region")
  result
}

#' Internal function to `collect()` assertions
#' @noRd
#' @keywords Internal
collect_assertions <- function(.query){
  if(!is.null(.query$data)){
    result <- .query$data |>
      parse(text = _) |> 
      eval()
    attr(result, "call") <- "assertions" # needed for `show_values()` to work
    attr(result, "region") <- potions::pour("atlas", "region") # needed for caching to work
  }else{
    result <- purrr::map(query_API(.query), 
                         \(a){a[names(a) != "termsRequiredToTest"]}) |>
      dplyr::bind_rows()
    names(result) <- rename_columns(names(result), type = "assertions")
    result <- result[wanted_columns("assertions")]
    result$type <- "assertions"
    attr(result, "call") <- "assertions" # needed for `show_values()` to work
    attr(result, "region") <- potions::pour("atlas", "region") # needed for caching to work    
    check_internal_cache(assertions = result)
  }
  result
}

#' Internal function to `collect()` atlases
#' @noRd
#' @keywords Internal
collect_atlases <- function(.query){
  result <- .query$data |>
    parse(text = _) |> 
    eval()
  attr(result, "call") <- "atlases"
  attr(result, "region") <- potions::pour("atlas", "region")
  result
}

#' Internal function to `collect()` collections
#' @noRd
#' @keywords Internal
collect_collections <- function(.query){
  if(is_gbif()){
    result <- query_API(.query)
    if(any(names(result) == "results")){ # happens when `filter()` not specified
      # Note: This assumes only one API call; will need more potentially
      result <- purrr::pluck(result, "results")
    }
    result <- flat_lists_only(result) |>
      dplyr::bind_rows()
  }else if(potions::pour("atlas", "region", .pkg = "galah") == "France"){
    result <- query_API(.query) |>
      purrr::pluck("_embedded", "producers") |>
      unlist()
    result <- tibble::tibble(name = result)
  }else{
    result <- query_API(.query) |> 
      dplyr::bind_rows() 
    result_reordered <- dplyr::relocate(result, "uid") 
    result <- result_reordered |> 
      dplyr::rename("id" = "uid")
  }
  attr(result, "call") <- "collections"
  attr(result, "region") <- potions::pour("atlas", "region")
  result
}

#' Internal function to remove `list()` entries inside lists
#' This supports passing to `bind_rows()`, but loses data
#' @noRd
#' @keywords Internal
flat_lists_only <- function(x){
  purrr::map(x, 
             \(a){
               purrr::map(a, \(b){
                 if(is.list(b)){
                   NULL
                 }else{
                   b
                 }
               })
             })
}

#' Internal function to `collect()` datasets
#' @noRd
#' @keywords Internal
collect_datasets <- function(.query){
  result <- query_API(.query)
  if(is_gbif()){
    if(any(names(result) == "results")){ # happens when `filter()` not specified
      # Note: This assumes only one API call; will need more potentially
      result <- purrr::pluck(result, "results")
    }
    result <- result |>
      flat_lists_only() |>
      dplyr::bind_rows()
  }else if(potions::pour("atlas", "region", .pkg = "galah") == "France"){
    result <- result |> 
      purrr::pluck("_embedded", "datasets") |>
      dplyr::bind_rows()
  }else{
    result <- result |> 
      dplyr::bind_rows() |>
      dplyr::relocate("uid") |>
      dplyr::rename("id" = "uid")
  }
  attr(result, "call") <- "datasets"
  attr(result, "region") <- potions::pour("atlas", "region") 
  result 
}

#' Internal function to `collect()` distributions
#' @noRd
#' @keywords Internal
collect_distributions_metadata <- function(.query){
  result <- query_API(.query)
  result <- result |>
    dplyr::bind_rows() |>
    dplyr::select(
      "spcode", 
      "family", 
      "genus_name", 
      "scientific", 
      "common_nam",
      "lsid",
      "area_name",
      "area_km",
      "data_resource_uid") |>
    dplyr::rename(
      "id" = "spcode", # this is chosen as ID because it is called by later APIs
      "genus" = "genus_name",
      "species" = "scientific",
      "taxon_concept_id" = "lsid",
      "label" = "area_name",
      "common_name" = "common_nam")  |>
    dplyr::mutate("common_name" = trimws(.data$common_name)) # remove leading or trailing spaces
  attr(result, "call") <- "distributions"
  attr(result, "region") <- potions::pour("atlas", "region") 
  result 
}

#' Internal function to `collect()` fields
#' @noRd
#' @keywords Internal
collect_fields <- function(.query){
  if(is_gbif()){
    result <- .query$data |>
      parse(text = _) |> 
      eval()
    attr(result, "call") <- "fields"
    attr(result, "region") <- potions::pour("atlas", "region")
    result
  }else{
    if(!is.null(.query$url)){ # i.e. there is no cached `tibble`
      result <- query_API(.query) |>
        dplyr::bind_rows() 
      # if there is a 'stored' field, use it to filter results
      if(any(colnames(result) == "stored")){
        result <- result |> 
          dplyr::filter(.data$stored == TRUE)
      }
      # now mutate to required format
      result <- result |>
        dplyr::mutate(id = result$name) |>
        dplyr::select(dplyr::all_of(wanted_columns("fields"))) |>
        dplyr::mutate(type = "fields") |>
        dplyr::bind_rows(galah_internal_archived$media,
                         galah_internal_archived$other)
      attr(result, "call") <- "fields"
      attr(result, "region") <- potions::pour("atlas", "region")
      check_internal_cache(fields = result)
      result
    }else{ # this should only happen when `data` slot is present in place of `url`
      check_internal_cache()[["fields"]]
    }
  }
}

#' Internal function to `collect()` licences
#' @noRd
#' @keywords Internal
collect_licences <- function(.query){
  result <- query_API(.query) 
  if(length(result) > 0){
    if (any(duplicated(names(result[[1]])))) { # remove duplicate columns (i.e. Spain atlas)
      result <- purrr::map(result, \(x) x[unique(names(x))])
    }
    result <- result |> 
      dplyr::bind_rows() |>
      dplyr::select(dplyr::all_of(c("id", "name", "acronym", "url"))) |> 
      dplyr::arrange(result$id)
  }else{
    result <- tibble::tibble(id = character(),
                             name = character(),
                             acronym = character(),
                             url = character())
  }
  attr(result, "call") <- "licences"
  attr(result, "region") <- potions::pour("atlas", "region") 
  result
}

#' Internal function to `collect()` lists
#' @noRd
#' @keywords Internal
collect_lists <- function(.query){
  if(inherits(.query$url, "data.frame")){
    result <- purrr::map(query_API(.query), 
                        \(a){a$lists}) |>
      dplyr::bind_rows()    
  }else{
    result <- query_API(.query) |>
      purrr::pluck("lists") |>
      dplyr::bind_rows()
  }
  if(any(colnames(result) == "dataResourceUid")){
    result <- result |>
      dplyr::rename("species_list_uid" = "dataResourceUid")    
  }
  attr(result, "call") <- "lists"
  attr(result, "region") <- potions::pour("atlas", "region") 
  result
}

#' Internal function to `collect()` profiles
#' @noRd
#' @keywords Internal
collect_profiles <- function(.query){
  if(!is.null(.query$url)){
    result <- query_API(.query) |>
      dplyr::bind_rows() 
    result <- result |>
      dplyr::filter(!duplicated(result$id)) |>
      dplyr::arrange("id") |>
      dplyr::select(dplyr::all_of(wanted_columns(type = "profile")))
    attr(result, "call") <- "profiles"
    attr(result, "region") <- potions::pour("atlas", "region") 
    check_internal_cache(show_all_profiles = result)
    result
  }else{
    check_internal_cache()[["profiles"]]
  }
}

#' Internal function to `collect()` providers
#' @noRd
#' @keywords Internal
collect_providers <- function(.query){
  result <- query_API(.query)
  if(is_gbif()){
    if(any(names(result) == "results")){ # happens when `filter()` not specified
      # Note: This assumes only one API call; will need more potentially
      result <- purrr::pluck(result, "results")
    }
    result <- result |>
      flat_lists_only() |>
      dplyr::bind_rows()
  }else if(potions::pour("atlas", "region", .pkg = "galah") == "France"){
    result <- tibble::tibble(name = {
      purrr::pluck(result, "_embedded", "providers") |> 
        unlist()
      })
  }else{
    result <- result |> 
      dplyr::bind_rows()
    if(nrow(result) > 0){ # exception added because this API isn't always populated (e.g. France)
      result <- result |> 
        dplyr::relocate("uid") |> 
        dplyr::rename("id" = "uid")      
    }
  }
  attr(result, "call") <- "providers"
  attr(result, "region") <- potions::pour("atlas", "region")
  result
}

#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_ranks <- function(.query){
  result <- .query$data |>
    parse(text = _) |> 
    eval()
  attr(result, "call") <- "ranks"
  attr(result, "region") <- potions::pour("atlas", "region")
  result
}

#' Internal function to `collect()` reasons
#' @noRd
#' @keywords Internal
collect_reasons <- function(.query){
  if(!is.null(.query$url)){
    result <- query_API(.query) |> 
      dplyr::bind_rows() 
    result <- result |>
      dplyr::filter(!result$deprecated) |>
      dplyr::select(dplyr::all_of(wanted_columns("reasons"))) |>
      arrange("id")
    attr(result, "call") <- "reasons"
    attr(result, "region") <- potions::pour("atlas", "region") 
    check_internal_cache(reasons = result)
    result
  }else{
    check_internal_cache()[["reasons"]]
  }
}
