#' Convert an object to class `query`
#'
#' Functionally similar to [collapse()], but without passing through 
#' [coalesce()] first. Primarily an internal function, but exported for 
#' clarity and debugging purposes.
#' @details
#' Typically, queries in galah are piped using `galah_call()`, which builds
#' an object of class `"data_request"`, `"metadata_request"` or `"files_request"`. 
#' This parses to an object of class `"query"` via [collapse()]. However,
#' [collapse()] first calls [coalesce()], which expands to a `query_set` 
#' _before_ evaluating [collapse()]. In this context, `as_query()` serves two
#' functions: externally, it can be called to convert directly to a `query` 
#' without running checks; and internally it allows a query to be appended 
#' to a `query_set` without calling [collapse()], which would begin an 
#' infinite loop (because `collapse()` calls `coalesce()`).
#' 
#' For simple cases, this gives the same result as running 
#' [collapse()] while the `run_checks` argument of [galah_config()] is set to 
#' `FALSE`, but is slightly faster. For complex cases, however, it is likely
#' to generate irresolvable API calls, because e.g. taxonomic queries are not
#' parsed before the URL is built. It should therefore be used with care.
#' @name as_query.data_request
#' @order 1
#' @export
as_query <- function(x, ...){
  UseMethod("as_query")
}

#' @rdname as_query.data_request
#' @order 2
#' @export
as_query.data_request <- function(x, ...){
  switch(x$type,
         "occurrences" = {
           if(is.null(x$group_by)){
             as_query_occurrences(x)  
           }else{
             as_query_species(x)
           }
         },
         "occurrences-count" = as_query_occurrences_count(x),
         "occurrences-doi" = as_query_occurrences_doi(x),
         "species" = as_query_species(x),
         "species-count" = as_query_species_count(x),
         "distributions" = as_query_distributions_data(x),
         cli::cli_abort("unrecognised 'type'")) |>
    structure(class = c("query", "list"))
}

#' @rdname as_query.data_request
#' @order 3
#' @export
as_query.metadata_request <- function(x, ...){
  switch(x$type,
         "apis" = as_query_apis(),
         "assertions" = as_query_assertions(),
         "atlases" = as_query_atlases(),
         "collections" = as_query_collections(x),
         "datasets" = as_query_datasets(x),
         "distributions" = as_query_distributions_metadata(x),
         "fields" = as_query_fields(),
         "fields-unnest" = as_query_fields_unnest(x),
         "licences" = as_query_licences(),
         "lists" = as_query_lists(x),
         "lists-unnest" = as_query_lists_unnest(x),
         "media" = as_query_media_metadata(x),
         "profiles" = as_query_profiles(),
         "profiles-unnest" = as_query_profiles_unnest(x),
         "providers" = as_query_providers(x),
         "ranks" = as_query_ranks(),
         "reasons" = as_query_reasons(),
         "taxa" = as_query_taxa(x),
         "taxa-unnest" = as_query_taxa_unnest(x),
         "identifiers" = as_query_identifiers(x),
         cli::cli_abort("unrecognised 'type'")
         ) |>
    structure(class = c("query", "list"))
}

#' @rdname as_query.data_request
#' @order 4
#' @export
as_query.files_request <- function(x, ...){
  browser()
}