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
#' **For more information about taxonomic searches using `search_taxa()`, see `?taxonomic_searches`**.
#' 
#' `r lifecycle::badge("stable")`
#' `search_all()` is a helper function that can do searches for multiple 
#' types of information, acting as a wrapper around many `search_` sub-functions. 
#' See `Details` (below) for accepted values.
#' 
#' @param type A string to specify what type of parameters should be searched.
#' @param query A string specifying a search term. Searches are not 
#' case-sensitive.
#' @param ... A set of strings or a tibble to be queried; see 
#' Details.
#' @details There are five categories of information, each with their own 
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
#' @importFrom rlang as_name
#' @importFrom rlang .data
#' @export
search_all <- function(type, query){
  
  # vector of valid types for this function
  valid_types <- c(
    "apis",
    "assertions",
    "atlases",
    "collections",
    "datasets",
    "fields",
    "licences",
    "lists",
    "identifiers",
    "profiles",
    "providers",
    "reasons",
    "taxa",
    "ranks")
  
  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- parse_quosures_basic(enquos(type))
    if(!inherits(type, "character") | length(type) > 1){
      abort("`type` must be a length-1 vector of class 'character'")
    }
    check_type_valid(type, valid_types)   
  }
  
  check_if_missing(query)
  
  if(type == "taxa"){
    check_if_in_pipe(query)
    request_metadata(type = "taxa") |>
      identify(query[[1]]) |>
      collect()
  }else if(type == "identifiers"){
    request_metadata() |>
      filter("identifier" == query) |>
      collect()
  }else{
    if(is_gbif() & 
       type %in% c("collections", "datasets", "providers")){ # these support `q` arg in API
      request_metadata() |>
        filter({{type}} == query) |>
        collect()
    }else{
      request_metadata(type = type) |> 
        collect() |>
        search_text_cols(query = query) 
    }
  }
}

#' Internal function to run a query over a tibble
#' @importFrom dplyr filter
#' @importFrom purrr list_transpose
#' @noRd
#' @keywords Internal
search_text_cols <- function(df, query){

  query <- tolower(query)
  keep_cols <- unlist(lapply(df, is.character)) & 
    colnames(df) != "type"
  check_list <- lapply(df[, keep_cols], 
                       function(a){grepl(query, tolower(a))})
  check_vector <- lapply(list_transpose(check_list), any) |> unlist()
  result <- df |> filter({{check_vector}})
  
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
#' @importFrom rlang abort
check_if_missing <- function(query,
                             parent_function,
                             error_call = caller_env()) {
  if (missing(query)) {
    bullets <- c(
      "We didn't detect a search query.",
      i = "Try entering a string to search for matching values."
    )
    abort(bullets, call = error_call)
  }
}

#' Internal function to check if `search_taxa()` has been piped in a 
#' `galah_call()` instead of `galah_identify()`
#' @noRd
#' @keywords Internal
check_if_in_pipe <- function(..., error_call = caller_env()) {
  dots <- list(...)
  col_type_present <- grepl("type", names(unlist(dots)))
  if (any(col_type_present)) {
    bullets <- c(
      "Can't pipe `search_taxa()` in a `galah_call()`.",
      i = "Did you mean to use `galah_identify()`?"
    )
    abort(bullets, call = error_call)
  }
}

#' @rdname search_all
#' @export
search_assertions <- function(query){search_all("assertions", query)}

#' @rdname search_all
#' @export
search_apis <- function(query){search_all("apis", query)}

#' @rdname search_all
#' @export
search_atlases <- function(query){search_all("atlases", query)}

#' @rdname search_all
#' @export
search_collections <- function(query){search_all("collections", query)}

#' @rdname search_all
#' @export
search_datasets <- function(query){search_all("datasets", query)}

#' @rdname search_all
#' @export
search_fields <- function(query){search_all("fields", query)}

#' @rdname search_all
#' @export
search_identifiers <- function(...){search_all("identifiers", unlist(list(...)))}

#' @rdname search_all
#' @export
search_licences <- function(query){search_all("licences", query)}

#' @rdname search_all
#' @export
search_lists <- function(query){search_all("lists", query)}

#' @rdname search_all
#' @export
search_profiles <- function(query){search_all("profiles", query)}

#' @rdname search_all
#' @export
search_providers <- function(query){search_all("providers", query)}

#' @rdname search_all
#' @export
search_ranks <- function(query){search_all("ranks", query)}

#' @rdname search_all
#' @export
search_reasons <- function(query){search_all("reasons", query)}
 
#' @rdname search_all
#' @export
search_taxa <- function(...){search_all("taxa", list(...))}