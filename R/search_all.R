#' @name search_all
#' @rdname search_all
#' @title Search for record information
#' @description 
#' The living atlases store a huge amount of information, above and beyond the 
#' occurrence records that are their main output. In `galah`, one way that 
#' users can investigate this information is by searching for a specific option 
#' or category for the type of information they are interested in. 
#' Functions prefixed with `search_` do this, displaying any matches to a 
#' search term within the valid options for the information specified by the 
#' suffix.
#' 
#' **For more information about taxonomic searches using `search_taxa()`, see**
#' \code{\link[=taxonomic_searches]{?taxonomic_searches}}.
#' 
#' `r lifecycle::badge("stable")`
#' `search_all()` is a helper function that can do searches for multiple 
#' types of information, acting as a wrapper around many `search_` sub-functions. 
#' See `Details` (below) for accepted values.
#' 
#' @param type A string to specify what type of parameters should be searched.
#' @param query A string specifying a search term. Searches are not 
#' case-sensitive.
#' @param all_fields `r lifecycle::badge("experimental")` If `TRUE`, 
#'   `show_values()` also returns all columns available from the API, rather
#'   than the 'default' columns traditionally provided via galah.
#' @details There are six categories of information, each with their own 
#' specific sub-functions to look-up each type of information. 
#' The available types of information for `search_all()` are:
#' 
#' 
#' | **Category** | **Type** | **Description** | **Sub-functions** |
#' |---|---|---|---|
#' | configuration  |`atlases`| Search for what atlases are available | `search_atlases()`|
#' | |`apis`| Search for what APIs & functions are available for each atlas | `search_apis()`|
#' | |`reasons`| Search for what values are acceptable as 'download reasons' for a specified atlas | `search_reasons()`|
#' | taxonomy | `taxa` | Search for one or more taxonomic names | `search_taxa()` |
#' | |`identifiers`| Take a universal identifier and return taxonomic information | `search_identifiers()` |
#' | |`ranks`| Search for valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.) | `search_ranks()` |
#' | filters |`fields`| Search for fields that are stored in an atlas | `search_fields()` |
#'  | |`assertions`| Search for results of data quality checks run by each atlas | `search_assertions()` |
#' | |`licenses`| Search for copyright licences applied to media | `search_licenses()`|
#' |group filters|`profiles`| Search for what data profiles are available | `search_profiles()` |
#' | |`lists`| Search for what species lists are available| `search_lists()` |
#' |data providers|`providers`| Search for which institutions have provided data | `search_providers()`|
#' | |`collections`|Search for the specific collections within those institutions| `search_collections()`|
#' | |`datasets`|Search for the data groupings within those collections| `search_datasets()`|  
#' |media|`media`|Search for images or sounds using a vector of IDs|`search_media()`|
#' 
#' 
#' @aliases search_all
#' @aliases search_atlases search_reasons
#' @aliases search_ranks search_fields search_assertions 
#' @aliases search_profiles search_providers 
#' @aliases search_collections search_datasets search_licences search_apis
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) 
#' containing all data that match the search query.
#' @seealso Use the [show_all()] function and `show_all_()` sub-functions to 
#' show available options of information. These functions are used to pass valid 
#' arguments to \code{\link[=filter.data_request]{filter()}}, 
#' \code{\link[=select.data_request]{select()}}, and related functions. 
#' Taxonomic queries are somewhat more involved; see [taxonomic_searches] for
#' details.
#' @examples \dontrun{
#' # Search for fields that include the word "date"
#' search_all(fields, "date")
#' 
#' # Search for fields that include the word "marine"
#' search_all(fields, "marine")
#' 
#' # Search using a single taxonomic term
#' # (see `?search_taxa()` for more information)
#' search_all(taxa, "Reptilia") # equivalent
#' 
#' # Look up a unique taxon identifier
#' # (see `?search_identifiers()` for more information)
#' search_all(identifiers, 
#'            "https://id.biodiversity.org.au/node/apni/2914510")
#' 
#' # Search for species lists that match "endangered"
#' search_all(lists, "endangered") # equivalent
#' 
#' # Search for a valid taxonomic rank, "subphylum"
#' search_all(ranks, "subphylum")
#' 
#' # An alternative is to download the data and then `filter` it. This is 
#' # largely synonymous, and allows greater control over which fields are searched.
#' request_metadata(type = "fields") |>
#'  collect() |>
#'  dplyr::filter(grepl("date", id))
#' }
#' @export
search_all <- function(type,
                       query,
                       all_fields = FALSE){
  
  # vector of valid types for this function
  valid_types <- c(
    "apis",
    "assertions",
    "atlases",
    "collections",
    "datasets",
    "fields",
    "identifiers",
    "licences",
    "lists",
    "media",
    "profiles",
    "providers",
    "reasons",
    "taxa",
    "ranks")
  
  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- rlang::enquos(type) |>
      parse_quosures_basic()
    if(!inherits(type, "character") | length(type) > 1){
      cli::cli_abort("`type` must be a length-1 vector of class 'character'")
    }
    check_type_valid(type, valid_types)   
  }
  
  check_if_missing(query)
  run_subsequent_query <- FALSE
  
  if(type == "taxa"){
    check_if_in_pipe(query)
    request <- request_metadata(type = "taxa") |>
      identify(query)
  }else if(type == "identifiers"){
    request <- request_metadata() |>
      filter("identifier" == query)
  }else if(type == "media"){
    request <- request_metadata() |>
      filter("media" == query)
  }else{
    if(is_gbif() & 
       type %in% c("collections", "datasets", "providers")){ # these support `q` arg in API
      request <- request_metadata() |>
        filter({{type}} == query)
    }else{
      request <- request_metadata(type = type)
      run_subsequent_query <- TRUE
    }
  }
  
  # add all_fields if requested
  if(isTRUE(all_fields)){
    request <- request |>
      dplyr::select(tidyselect::everything())
  }
  
  # collect the supplied `request`
  result <- collect(request)
  
  # add search, or not, depending on behaviour
  if(isTRUE(run_subsequent_query) & nrow(result) > 0){
    search_text_cols(result, query)  
  }else{
    result
  }
}

#' Internal function to run a query over a tibble
#' @noRd
#' @keywords Internal
search_text_cols <- function(df, query){

  if(nrow(df) < 1){
    return(df)
  }

  query <- tolower(query)
  keep_cols <- unlist(purrr::map(df, is.character)) & 
    colnames(df) != "type"

  check_list <- purrr::map(df[, keep_cols], 
                           \(a){stringr::str_detect(tolower(a), query)})
  
  if(length(check_list) < 1){
    return(df)
  }

  check_vector <- purrr::list_transpose(check_list) |>
    purrr::map(any) |> 
    unlist()
  result <- df |> dplyr::filter({{check_vector}})
  
  # order search_all() results
  if ("id" %in% colnames(result)) {
    similarity <- utils::adist(result$id, query)[, 1] # similarity of results to query
    result <- result[order(similarity), ] # reorder results
  }
  
  # return results in order of similarity to search term
  return(result)
}

#' Internal function to check for missingness
#' @noRd
#' @keywords Internal
check_if_missing <- function(query,
                             parent_function,
                             error_call = rlang::caller_env()) {
  if (missing(query)) {
    c(
      "We didn't detect a search query.",
      i = "Try entering a string to search for matching values.") |>
    cli::cli_abort(call = error_call)
  }
}

#' Internal function to check if `search_taxa()` has been piped in a 
#' `galah_call()` instead of `galah_identify()`
#' @noRd
#' @keywords Internal
check_if_in_pipe <- function(..., 
                             error_call = rlang::caller_env()){
  dots <- list(...)
  col_type_present <- grepl("type", names(unlist(dots)))
  if(any(col_type_present)){
    c("Can't pipe `search_taxa()` in a `galah_call()`.",
      i = "Did you mean to use `galah_identify()`?") |>
      cli::cli_abort(call = error_call)
  }
}

#' @rdname search_all
#' @export
search_assertions <- function(query,
                              all_fields = FALSE){
  search_all("assertions", query, all_fields)
}

#' @rdname search_all
#' @export
search_apis <- function(query,
                        all_fields = FALSE){
  search_all("apis", query, all_fields)
}

#' @rdname search_all
#' @export
search_atlases <- function(query,
                           all_fields = FALSE){
  search_all("atlases", query, all_fields)
}

#' @rdname search_all
#' @export
search_collections <- function(query,
                               all_fields = FALSE){
  search_all("collections", query, all_fields)
}

#' @rdname search_all
#' @export
search_datasets <- function(query,
                            all_fields = FALSE){
  search_all("datasets", query, all_fields)
}

#' @rdname search_all
#' @export
search_fields <- function(query,
                          all_fields = FALSE){
  search_all("fields", query, all_fields)
}

#' @rdname search_all
#' @export
search_identifiers <- function(...,
                               all_fields = FALSE){
  search_all("identifiers", 
             unlist(list(...)), 
             all_fields = all_fields)
}

#' @rdname search_all
#' @export
search_licences <- function(query,
                            all_fields  = FALSE){
  search_all("licences", query, all_fields)
}

#' @rdname search_all
#' @export
search_lists <- function(query,
                         all_fields = FALSE){
  search_all("lists", query, all_fields)
}

#' @rdname search_all
#' @export
search_media <- function(query,
                         all_fields = FALSE){
  search_all("media", query, all_fields)
}

#' @rdname search_all
#' @export
search_profiles <- function(query,
                            all_fields = FALSE){
  search_all("profiles", query, all_fields)
}

#' @rdname search_all
#' @export
search_providers <- function(query,
                             all_fields = FALSE){
  search_all("providers", query, all_fields)
}

#' @rdname search_all
#' @export
search_ranks <- function(query,
                         all_fields = FALSE){
  search_all("ranks", query, all_fields)
}

#' @rdname search_all
#' @export
search_reasons <- function(query,
                           all_fields = FALSE){
  search_all("reasons", query, all_fields)
}
 
#' @rdname search_all
#' @export
search_taxa <- function(...,
                        all_fields = FALSE){
  dots <- list(...)
  if(length(dots) == 1L){
    if(inherits(dots[[1]], "data.frame")){
      search_all("taxa", dots[[1]], all_fields)
    }else{
      search_all("taxa", dots, all_fields)
    }
  }else{
    search_all("taxa", dots, all_fields)
  }
}