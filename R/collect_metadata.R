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

#' Function to convert list entries in tibbles, supporting list-columns
#' for nested data
#' @param x a list-entry - i.e. we need to call 
#' purrr::map(list, tidy_list_columns) for this to work
#' @noRd
#' @keywords Internal
tidy_list_columns <- function(x){
  
  # tibble-ify list columns
  list_check <- purrr::map(x, is.list) |>
    unlist()
  if(any(list_check)){
    list_names <- names(x)[list_check]
    list_tibbles <- purrr::map(list_names,
                              \(a){
                                tibble::tibble({{a}} := list(x[[a]]))
                              })
  }else{
    list_tibbles <- NULL
  }

  # bind with non-list columns
  x[!list_check] |>
    make_nulls_safe() |>
    tibble::as_tibble() |>
    dplyr::bind_cols(list_tibbles) |>
    dplyr::select(!!!names(x)) # reorder columns to same as `x`
}

#' Internal function to ensure rows can be converted to tibble
#' This is important because some lists contain `NULL`
#' The function fixes it by converting to `NA`
#' @param x a single list, to be converted into a tibble row. use map() for multiple rows
#' @noRd
#' @keywords Internal
make_nulls_safe <- function(x){
  x_null <- purrr::map(x, is.null) |>
    unlist()
  if(any(x_null)){
    x[x_null] <- purrr::map(x[x_null], \(a){c(NA)})
  }
  x
}

# collect functions

#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_apis <- function(.query){
  retrieve_internal_data(.query) |>
    update_attributes(type = "apis") |>
    parse_select(.query)
}

#' Internal function to `collect()` assertions
#' @noRd
#' @keywords Internal
collect_assertions <- function(.query){
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    # get data from API
    result_api <- purrr::map(query_API(.query), 
                         \(a){a[names(a) != "termsRequiredToTest"]})
    # set up a pipe to transform results from the API
    result_df <- result_api |>
      dplyr::bind_rows() |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_rename(.query) |>
      dplyr::mutate(type = "assertions") |> # for consistency with `collect_fields()`
      update_attributes(type = "assertions")
    update_cache(assertions = result_df)
  }
  parse_select(result_df, .query) # always evaluate `select()` last
}

#' Internal function to `collect()` atlases
#' NOTE: This function does not cache anything because it *always* calls internal data
#' @noRd
#' @keywords Internal
collect_atlases <- function(.query){
  retrieve_internal_data(.query) |>
    update_attributes(type = "atlases") |>
    parse_select(.query)
}

#' Internal function to `collect()` collections
#' @noRd
#' @keywords Internal
collect_collections <- function(.query){
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    # Handle GBIF first
    if(is_gbif()){
      result <- query_API(.query)
      if(any(names(result) == "results")){ # happens when `filter()` not specified
        # Note: This assumes only one API call; will need more potentially
        result <- purrr::pluck(result, "results")
      }
      result_df <- result |>
        tidy_list_columns() |>
        dplyr::bind_rows()
    # Then France
    }else if(potions::pour("atlas", "region", .pkg = "galah") == "France"){
      result <- .query |>
        query_API() |>
        purrr::pluck("_embedded", "producers") |>
        unlist()
      result_df <- tibble::tibble(name = result)
    # Finally, all other Living Atlases
    }else{
      result <- .query |>
        query_API()
      result_df <- result |>
        dplyr::bind_rows() |>
        dplyr::relocate("uid") |>
        dplyr::rename("id" = "uid")
    }
    result_df <- result_df |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_arrange() |>
      update_attributes(type = "collections")
    update_cache(collections = result_df)
  }
  parse_select(result_df, .query)
}

#' Internal function to `collect()` datasets
#' @noRd
#' @keywords Internal
collect_datasets <- function(.query){
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    result <- query_API(.query)
    if(is_gbif()){
      if(any(names(result) == "results")){ # happens when `filter()` not specified
        # Note: This assumes only one API call; will need more potentially
        result <- purrr::pluck(result, "results")
      }
      result_df <- result |>
        tidy_list_columns() |>
        dplyr::bind_rows()
    }else if(potions::pour("atlas", "region", .pkg = "galah") == "France"){
      result_df <- result |> 
        purrr::pluck("_embedded", "datasets") |>
        dplyr::bind_rows()
    }else{
      result_df <- result |> 
        dplyr::bind_rows() |>
        dplyr::relocate("uid") |>
        dplyr::rename("id" = "uid")
    }
    result_df <- result_df |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_arrange() |>
      update_attributes(type = "datasets")
    update_cache(datasets = result_df)
  }
  parse_select(result_df, .query)
}

#' Internal function to `collect()` distributions
#' @noRd
#' @keywords Internal
collect_distributions_metadata <- function(.query){
  result <- query_API(.query)
  result <- result |>
    dplyr::bind_rows() |>
    # NOTE: This syntax should be integrated with `lookup_select_columns()` et al before shipping
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
    result_df <- retrieve_internal_data(.query)
  }else{
    result <- query_API(.query) |>
      dplyr::bind_rows() 
    
    if(is_gbif()){
      result_df <- result |>
        dplyr::mutate(id = .data$simpleName,
                      description = .data$qualifiedName,
                      type = "fields")
    }else{
      # if there is a 'stored' field, use it to filter results
      if(any(colnames(result) == "stored")){
        result_df <- result |> 
          dplyr::filter(.data$stored == TRUE)
      }
      # now mutate to required format
      result_df <- result_df |>
        dplyr::mutate(id = result_df$name,
                      type = "fields") |>
        dplyr::rename_with(camel_to_snake_case) |>
        dplyr::bind_rows(galah_internal_archived$media,
                         galah_internal_archived$other)       
    }
    result_df <- update_attributes(result_df, type = "fields")
    update_cache(fields = result_df)
  }
  parse_select(result_df, .query)
}

#' Internal function to `collect()` licences
#' @noRd
#' @keywords Internal
collect_licences <- function(.query){
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    result <- query_API(.query) 
    if(length(result) > 0){
      if (any(duplicated(names(result[[1]])))) { # remove duplicate columns (i.e. Spain atlas)
        result <- purrr::map(result, \(x) x[unique(names(x))])
      }
      result_df <- result |> 
        dplyr::bind_rows() |>
        dplyr::rename_with(camel_to_snake_case) |>
        parse_arrange()
    }else{
      result_df <- tibble::tibble(id = character(),
                                  name = character(),
                                  acronym = character(),
                                  url = character())
    }
    result_df <- update_attributes(result_df, type = "licences")
    update_cache(licences = result_df)
  }
  parse_select(result_df, .query)
}

#' Internal function to `collect()` lists
#' @noRd
#' @keywords Internal
collect_lists <- function(.query){
  # NOTE: this function has some quite versatile behaviour, so we need to 
  # explicitly control when caching does (and does not) happen.
  should_update_cache <- FALSE

  # requests for cached data use the `data` slot; check this first
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    # here we run and parse an API call
    result <- query_API(.query) # this when `url` is a single value or a tibble
    # pagination returns long lists
    if(length(result) > 1){
      result_df <- purrr::map(query_API(.query), 
                              \(a){a$lists}) |>
        dplyr::bind_rows()
      should_update_cache <- TRUE      
    }else{
      lists_slot <- purrr::pluck(result, "lists")
      # single list queries that use `filter()` don't have a `lists` slot
      if(is.null(lists_slot)){
        result_df <- result[[1]] |>
          make_nulls_safe() |>
          tibble::as_tibble()
      # but some queries do
      }else{
        result_df <- lists_slot |>
          dplyr::bind_rows()
        should_update_cache <- TRUE
      }
    }
    
    # cleaning
    result_df <- result_df |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_rename(.query) |>
      parse_arrange() |>
      update_attributes(type = "lists")
    
    if(should_update_cache){
      update_cache(lists = result_df)   
    }
  }
  # return
  parse_select(result_df, .query)
}

#' Internal version of `collect()` for `request_metadata(type = "media")`
#' @param object of class `data_response`, from `compute()`
#' @noRd
#' @keywords Internal
collect_media_metadata <- function(.query,
                                   error_call = rlang::caller_env()){
  result <- query_API(.query) 

  # ensure list-columns are imported correctly
  result_df <- purrr::map(result, tidy_list_columns) |>
    dplyr::bind_rows()

  # Select only the information we want
  # NOTE: this has no caching *on purpose*
  result_df |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    dplyr::filter(!is.na(.data$image_id)) |>
    parse_select(.query)
}

#' Internal function to `collect()` profiles
#' @noRd
#' @keywords Internal
collect_profiles <- function(.query){
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    result <- query_API(.query) |>
      dplyr::bind_rows() 
    result_df <- result |>
      dplyr::filter(!duplicated(.data$id)) |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_arrange() |>
      update_attributes(type = "profiles")
    update_cache(profiles = result_df)
  }
  parse_select(result_df, .query)
}

#' Internal function to `collect()` providers
#' @noRd
#' @keywords Internal
collect_providers <- function(.query){
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    result <- query_API(.query)
    if(is_gbif()){
      if(any(names(result) == "results")){ # happens when `filter()` not specified
        # Note: This assumes only one API call; will need more potentially
        result <- purrr::pluck(result, "results")
      }
      result_df <- result |>
        tidy_list_columns() |>
        dplyr::bind_rows()
    }else if(potions::pour("atlas", "region", .pkg = "galah") == "France"){
      result_df <- tibble::tibble(name = {
        purrr::pluck(result, "_embedded", "providers") |> 
          unlist()
      })
    }else{
      result_df <- result |> 
        dplyr::bind_rows()
      if(nrow(result_df) > 0){ # exception added because this API isn't always populated (e.g. France)
        result_df <- result_df |> 
          dplyr::relocate("uid") |> 
          dplyr::rename("id" = "uid")
      }
    }
    result_df <- result_df |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_arrange() |>
      update_attributes(type = "providers")
    update_cache(providers = result_df)
  }
  parse_select(result_df, .query)
}

#' Internal function to `collect()` APIs
#' @noRd
#' @keywords Internal
collect_ranks <- function(.query){
  retrieve_internal_data(.query) |>
    update_attributes(type = "ranks") |>
    parse_select(.query)
}

#' Internal function to `collect()` reasons
#' @noRd
#' @keywords Internal
collect_reasons <- function(.query){
  if(!is.null(.query$data)){
    result_df <- retrieve_internal_data(.query)
  }else{
    result <- query_API(.query) |> 
      dplyr::bind_rows() 
    result_df <- result |>
      dplyr::filter(!.data$deprecated) |>
      parse_arrange() |>
      dplyr::relocate("id", "name") |>
      update_attributes(type = "reasons")
    update_cache(reasons = result_df)
  }
  parse_select(result_df, .query)
}