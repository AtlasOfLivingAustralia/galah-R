# Utilities

#' Internal function to update attributes on a metadata tibble 
#' as per `galah` requirements
#' @noRd
#' @keywords Internal
update_attributes <- function(df, type){
  attr(df, "call") <- type # needed for `show_values()` to work
  attr(df, "region") <- potions::pour("atlas", "region") # needed for caching to work
  df
}

#' Often a `.query` will have a `data` slot in place of a `url`
#' This may call e.g. `retrieve_cache()` OR an internal object
#' Either way, the string will be a valid function call set in `as_query()`
#' Our job at this point is to parse that function, NOT just `retrieve_cache()`
#' @noRd
#' @keywords Internal
retrieve_internal_data <- function(.query){
  if(is.null(.query$data)){
    cli::cli_abort("Query is missing a `data` slot")
  }
  .query$data |>
    parse(text = _) |> 
    eval()
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

# collect functions

#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_apis <- function(.query){
  retrieve_internal_data(.query) |>
    update_attributes(type = "apis")
}

#' Internal function to `collect()` assertions
#' @noRd
#' @keywords Internal
collect_assertions <- function(.query){
  if(!is.null(.query$data)){
    result <- retrieve_internal_data(.query)
  }else{
    result <- purrr::map(query_API(.query), 
                         \(a){a[names(a) != "termsRequiredToTest"]}) |>
      dplyr::bind_rows()
    names(result) <- rename_columns(names(result), type = "assertions")
    result <- result[wanted_columns("assertions")]
    result$type <- "assertions"
    result <- update_attributes(result, type = "assertions")
    update_cache(assertions = result)
  }
  result
}

#' Internal function to `collect()` atlases
#' @noRd
#' @keywords Internal
collect_atlases <- function(.query){
  retrieve_internal_data(.query) |>
    update_attributes(type = "atlases")
}

#' Internal function to `collect()` collections
#' @noRd
#' @keywords Internal
collect_collections <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
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
    result <- update_attributes(result, type = "collections")
    update_cache(collections = result)
    result
  }
}

#' Internal function to `collect()` datasets
#' @noRd
#' @keywords Internal
collect_datasets <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
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
    result <- update_attributes(result, type = "datasets")
    update_cache(datasets = result)
    result
  }
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
  result <- update_attributes(result, type = "distributions")
  update_cache(distributions = result)
  result 
}

#' Internal function to `collect()` fields
#' @noRd
#' @keywords Internal
collect_fields <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
    result <- query_API(.query) |>
      dplyr::bind_rows() 
    
    if(is_gbif()){
      result <- result |>
        dplyr::mutate(id = .data$simpleName,
                      description = .data$qualifiedName,
                      type = "fields") |>
        dplyr::select("id", "description", "type")
      
    }else{
      
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
    }
    result <- update_attributes(result, type = "fields")
    update_cache(fields = result)
    result
  }
}

#' Internal function to `collect()` licences
#' @noRd
#' @keywords Internal
collect_licences <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
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
    result <- update_attributes(result, type = "licences")
    update_cache(licences = result)
    result
  }
}

#' Internal function to `collect()` lists
#' @noRd
#' @keywords Internal
collect_lists <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
    # here we run and parse an API call
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
    result <- update_attributes(result, type = "lists")
    update_cache(lists = result)
    result
  }
}

#' Internal function to `collect()` profiles
#' @noRd
#' @keywords Internal
collect_profiles <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
    result <- query_API(.query) |>
      dplyr::bind_rows() 
    result <- result |>
      dplyr::filter(!duplicated(result$id)) |>
      dplyr::arrange("id") |>
      dplyr::select(dplyr::all_of(wanted_columns(type = "profile")))
    result <- update_attributes(result, type = "profiles")
    update_cache(profiles = result)
    result
  }
}

#' Internal function to `collect()` providers
#' @noRd
#' @keywords Internal
collect_providers <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
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
    result <- update_attributes(result, type = "providers")
    update_cache(providers = result)
    result
  }
}

#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_ranks <- function(.query){
  retrieve_internal_data(.query) |>
    update_attributes(type = "ranks")
}

#' Internal function to `collect()` reasons
#' @noRd
#' @keywords Internal
collect_reasons <- function(.query){
  if(!is.null(.query$data)){
    retrieve_internal_data(.query)
  }else{
    result <- query_API(.query) |> 
      dplyr::bind_rows() 
    result <- result |>
      dplyr::filter(!result$deprecated) |>
      dplyr::select(dplyr::all_of(wanted_columns("reasons"))) |>
      arrange("id") |>
      update_attributes(type = "reasons")
    update_cache(reasons = result)
    result
  }
}