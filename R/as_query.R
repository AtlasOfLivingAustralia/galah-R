#' Convert an object to class `query`
#'
#' Functionally similar to \code{\link[=collapse.data_request]{collapse()}}, but 
#' without passing through [coalesce()] first. Primarily an internal function, 
#' but exported for clarity and debugging purposes.
#' @details
#' Typically, queries in galah are piped using [galah_call()], which builds
#' an object of class `"data_request"`, `"metadata_request"` or 
#' `"files_request"`. All these objects can be converted to class `"query"` 
#' using \code{\link[=collapse.data_request]{collapse()}}. However,
#' \code{\link[=collapse.data_request]{collapse()}} first calls
#' \code{\link[=coalesce.data_request]{coalesce()}}, which expands to an
#' object of class `"query_set"` _before_ evaluating 
#' \code{\link[=collapse.data_request]{collapse()}}. In this context, 
#' [as_query()] serves two purposes: externally, it can be called to convert 
#' directly to class `"query"` without running checks; and internally it allows 
#' a query to be appended to a `"query_set"` without calling causing an 
#' infinite loop.
#' 
#' For simple cases, this gives the same result as running 
#' \code{\link[=collapse.data_request]{collapse()}} while the `run_checks` 
#' argument of [galah_config()] is set to `FALSE`, but is slightly faster. For 
#' complex cases, however, it is likely to generate irresolvable API calls, 
#' because e.g. taxonomic queries are not parsed before the URL is built. It 
#' should therefore be used with care.
#' @name as_query.data_request
#' @param x An object to convert to a `query`. Supported classes are the same
#' as those produced by [galah_call()], namely `data_request`, 
#' `metadata_request` or `files_request`.
#' @param ... Other arguments, currently ignored
#' @order 1
#' @return An object of class `query`, which is a list-like object containing 
#' two or more of the following slots:
#' 
#'  - `type`: The type of query, serves as a lookup to the corresponding field in `show_all(apis)`
#'  - `url`: Either:
#'    - a length-1 character giving the API to be queried; or 
#'    - a `tibble()` containing at least the field `url` and optionally others
#'  - `headers`: headers to be sent with the API call
#'  - `body`: body section of the API call
#'  - `options`: options section of the API call
#'  - Any other information retained from the preceeding `_request` object (see [galah_call()])
#'
#' @seealso To open a piped query, see [galah_call()]. For alternative 
#' operations on `_request` objects, see [coalesce()], 
#' \code{\link[=collapse.data_request]{collapse()}}, 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @export
as_query <- function(x, ...){
  UseMethod("as_query")
}

#' @rdname as_query.data_request
#' @param mint_doi Logical: should a DOI be minted for this download? Only 
#' applies to `type = "occurrences"` when atlas chosen is "ALA".
#' @order 2
#' @export
as_query.data_request <- function(x,
                                  mint_doi = FALSE,
                                  ...){
  x <- check_authentication(x)
  switch(x$type,
         "occurrences" = {
           if(is.null(x$group_by)){
             as_query_occurrences(x, mint_doi = mint_doi)  
           }else{
             as_query_species(x)
           }
         },
         "occurrences-count" = as_query_occurrences_count(x),
         "occurrences-doi" = as_query_occurrences_doi(x),
         "species" = as_query_species(x),
         "species-count" = as_query_species_count(x),
         "distributions" = as_query_distributions_data(x),
         cli::cli_abort("Unrecognised 'type'")) |>
    retain_authentication(source = x)
}

#' @rdname as_query.data_request
#' @order 3
#' @export
as_query.metadata_request <- function(x, ...){
  x <- check_authentication(x)
  switch(x$type,
         "apis" = as_query_apis(x),
         "assertions" = as_query_assertions(x),
         "atlases" = as_query_atlases(x),
         "collections" = as_query_collections(x),
         "config" = as_query_config(x),
         "datasets" = as_query_datasets(x),
         "distributions" = as_query_distributions_metadata(x),
         "fields" = as_query_fields(x),
         "fields-unnest" = as_query_fields_unnest(x),
         "licences" = as_query_licences(x),
         "lists" = as_query_lists(x),
         "lists-unnest" = as_query_lists_unnest(x),
         "media" = as_query_media_metadata(x),
         "profiles" = as_query_profiles(x),
         "profiles-unnest" = as_query_profiles_unnest(x),
         "providers" = as_query_providers(x),
         "ranks" = as_query_ranks(x),
         "reasons" = as_query_reasons(x),
         "taxa" = as_query_taxa(x),
         "taxa-unnest" = as_query_taxa_unnest(x),
         "identifiers" = as_query_identifiers(x),
         cli::cli_abort("Unrecognised 'type'")
         ) |>
    retain_authentication(source = x)
}

#' @rdname as_query.data_request
#' @param thumbnail Logical: should thumbnail-size images be returned? Defaults 
#' to `FALSE`, indicating full-size images are required.
#' @order 4
#' @export
as_query.files_request <- function(x, 
                                   thumbnail = FALSE,
                                   ...){
  # NOTE: switch is technically superfluous right now, but could be useful
  # for future file types
  
  # This code is identical to `collapse.files_request()`
  switch(x$type,
         "media" = as_query_media_files(x, 
                                        thumbnail = thumbnail),
         cli::cli_abort("Unrecognised 'type'")) |>
    retain_authentication(source = x)
}

#' @rdname as_query.data_request
#' @order 5
as_query.list <- function(x){
  # TODO add some checks here?
  structure(x, class = c("query", "list"))
}

#' @rdname as_query.data_request
#' @order 6
as_query.query <- function(x){
  x
}