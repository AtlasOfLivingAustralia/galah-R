#' Internal function to run `compute()` for 
#' `request_metadata(type = "fields") |> unnest()`
#' @noRd
#' @keywords Internal
collect_fields_unnest <- function(.query,
                                  error_call = rlang::caller_env()){
  facet <- .query |>
    purrr::pluck("url") |>
    httr2::url_parse()

  if(.query$atlas == "Global"){
    # get name of facet in question
    facet <-  purrr::pluck(facet, "query", "facet") # NOTE: "facet" (singular)
    check_missing_fields(facet, call = error_call)
    # get result from API
    result <- .query |>
      query_API() |>
      purrr::pluck(!!!list("facets", 1, "counts")) 
    # add an error catcher here, as next step is sensitive to column missingness
    # this should be caught because `pluck()` generates NULL for missing data
    if(is.null(result)){
      tibble::tibble()
    }else{
      result |>
      dplyr::bind_rows() |>
      dplyr::rename_with(camel_to_snake_case) |>
      dplyr::rename({{facet}} := "name") |>
      parse_select(.query)
    }
  }else{ 
    facet <-  purrr::pluck(facet, "query", "facets") # NOTE: "facets" (plural)
    check_missing_fields(facet, call = error_call)
    result <- .query |>
      query_API() |>
      purrr::pluck(!!!list(1, "fieldResult")) |>
      dplyr::bind_rows()
    
    # extract unformatted facet values
    if(nrow(result) > 0){
      result |>
        dplyr::mutate(
          field_name = stringr::str_extract(.data$i18nCode, "(?<=\\.).*"),
          .before = 1) |>
        dplyr::rename_with(camel_to_snake_case) |>
        dplyr::rename({{facet}} := "field_name") |>
        parse_select(.query)
    }else{ # i.e. catch empty results
      result
    }
  }
}

#' Microfunction to prevent later failures due to missing field names
#' @noRd
#' @keywords Internal
check_missing_fields <- function(x, call){
  if (x == "NA") {
    cli::cli_abort("No `field` passed to `show_values()`/`search_values()`.",
                   call = call)
  }
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "lists") |> unnest()`
#' @noRd
#' @keywords Internal
collect_lists_unnest <- function(.query){

  clean_common_names <- function(df){
    if(any(colnames(df) == "commonName")){
      df$commonName <- as.character(df$commonName)
      if(any(df$commonName == "NULL")){
        df$commonName[df$commonName == "NULL"] <- NA
      }
    }
    df
  }

  # extract additional raw fields columns
  clean_kvp_values <- function(df){
    if(any(colnames(df) == "kvpValues")){
      if(any(lengths(df$kvpValues) > 0)){
        df <- df |>
          tidyr::unnest(cols = "kvpValues") |>
          tidyr::unnest_wider("kvpValues") |>
          tidyr::pivot_wider(names_from = "key",
                             values_from = "value")
      }
    }
    df
  }
  
  # get data
  result <- query_API(.query)
  
  if(stringr::str_detect(.query$url, "/v1/")) { # version 1 API
    # old
    result |>
      purrr::list_transpose() |>
      tibble::as_tibble() |>
      clean_common_names() |>
      clean_kvp_values() |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_rename(.query) |>
      parse_select(.query)
  } else {
    # new
    # first, add `classification` which captures ALA-matched information
    x <- result |>
      purrr::list_transpose() |>
      tibble::as_tibble() |>
      clean_common_names() |>
      # species_list_uid is no longer retained in v2 API results.
      # this adds info again as column (which matches old v1 output)
      dplyr::mutate(species_list_uid = .query$request$filter$value) |> 
      parse_classification() |>
      dplyr::rename_with(camel_to_snake_case) |>
      parse_rename(.query)
    
    # second, `properties` contains status information and other raw fields
    raw_columns <- x |>
      select(taxon_concept_id, supplied_name, scientific_name, properties) |>
      tidyr::unnest(cols = properties) |>
      tidyr::unnest_wider(properties) |>
      dplyr::mutate(key = camel_to_snake_case(key)) |>
      dplyr::mutate(key = dplyr::if_else(key %in% colnames(x), glue::glue("{key}_raw"), key)) |> # rename prior to pivot to avoid name conflicts
      tidyr::pivot_wider(names_from = "key",
                         values_from = "value",
                         names_repair = "minimal",
                         values_fn = list) |>
      tidyr::unnest(cols = everything())
    
    # merge
    result_final <- x |>
      dplyr::left_join(raw_columns, 
                       dplyr::join_by(taxon_concept_id, supplied_name, scientific_name)) |>
      dplyr::select(-properties) |>
      parse_rename(.query) 
      # parse_select(.query) # FIXME: this is limiting output in an unexpected way
    
    # inform user about duplicated taxon_concept_ids
    duplicate_taxa <- result_final |> filter(dplyr::n() > 1, .by = taxon_concept_id) |> distinct(taxon_concept_id) |> nrow()
    bullets <- c("List contains {duplicate_taxa} taxon_concept_id(s) with > 1 row.",
                 "i" = "This happens because {.field taxon_concept_id} can match to multiple {.field supplied_name} values.",
                 "i" = "To see duplicated rows, save list as object then run: {.code {{your_object}} |> dplyr::filter(dplyr::n() > 1, .by = taxon_concept_id)}")
    cli::cli_warn(bullets)
    
    return(result_final)
  }

}

#' Internal function to parse ALA-matched taxonomic information of species lists 
#' via `show_values()` / `request_metadata(type = "lists") |> unnest()`
#' @noRd
#' @keywords Internal
parse_classification <- function(df){
  
  if(any(colnames(df) == "classification")){
      df <- df |>
        tidyr::unnest_wider(classification, 
                                   names_repair = "minimal", 
                                   names_sep = "_") |> 
        # select ALA-matched columns
        select(species_list_uid, 
               classification_taxonConceptID, 
               suppliedName,
               # scientific_name,
               classification_scientificName, # ALA-matched name
               classification_scientificNameAuthorship, 
               classification_vernacularName,
               classification_rank, 
               classification_kingdom, 
               classification_phylum, 
               classification_class, 
               classification_order, 
               classification_family, 
               classification_genus, 
               classification_species,
               properties # keep properties for raw fields
               ) |>
        # remove prefix
        dplyr::rename_with( 
          ~ stringr::str_remove(., "classification_"), 
          classification_taxonConceptID:classification_species
        )
  }
  df
  

}



#' Internal function to run `compute()` for 
#' `request_metadata(type = "profiles") |> unnest()`
#' @noRd
#' @keywords Internal
collect_profiles_unnest <- function(.query){
  result <- query_API(.query)
  result |>
    purrr::pluck("categories") |>
    dplyr::bind_rows() |>
    dplyr::pull("qualityFilters") |>
    dplyr::bind_rows() |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_select(.query)
}

#' Internal function to run `compute()` for 
#' `request_metadata(type = "taxa") |> unnest()`
#' @noRd
#' @keywords Internal
collect_taxa_unnest <- function(.query){
  query_API(.query) |>
    dplyr::bind_rows() |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    parse_select(.query)

  ## if useful to retain supplied taxon, do so here
  # supplied_df <- .query$supplied_taxon |>
  #  dplyr::select(tidyselect::any_of(colnames(result_df)))
  # dplyr::bind_rows(supplied_df, result_df)
}
