#' Narrow a query by passing taxonomic identifiers
#'
#' When conducting a search or creating a data query, it is common to identify 
#' a known taxon or group of taxa to narrow down the records or results returned. 
#'
#' `galah_identify()` is used to identify taxa you want returned in a search or 
#' a data query. Users to pass scientific names or taxonomic identifiers
#' with pipes to provide data only for the biological group of interest. 
#' 
#' It is good to use [search_taxa()] and [search_identifiers()] 
#' first to check that the taxa you provide to `galah_identify()` return the 
#' correct results.
#'
#' @param ... one or more scientific names (if `search = TRUE`) or taxonomic 
#'   identifiers (if `search = FALSE`); or an object of class `ala_id` (from
#'   `search_taxa`).
#' @param search (logical); should the results in question be passed to
#'   `search_taxa`?
#' @return A tibble containing identified taxa.
#' @seealso [search_taxa()] to find identifiers from scientific names;
#' [search_identifiers()] for how to get names if taxonomic identifiers 
#' are already known.
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @examples
#' # Specify a taxon. A valid taxon will return an identifier.
#' galah_identify("reptilia")
#' 
#' # Specify more than one taxon at a time.
#' galah_identify("reptilia", "mammalia", "aves", "pisces")
#' 
#' # Use `galah_identify()` to narrow your queries
#' galah_call() |> 
#'   galah_identify("Eolophus") |>
#'   atlas_counts()
#' 
#' # If you know a valid taxon identifier, use `galah_filter()` instead.
#' # This was formerly supported by `galah_identify()` with `search = FALSE`
#' galah_call() |> 
#'   galah_filter(lsid == "https://biodiversity.org.au/afd/taxa/009169a9-a916-40ee-866c-669ae0a21c5c") |>
#'   atlas_counts()
#' @importFrom rlang warn
#' @export
galah_identify <- function(...) {
  dots_initial <- list(...)
  if (length(dots_initial) < 1) {
    warn("No query passed to `identify()`")
    tibble("search_term" = character())
  }else{
    if(inherits(dots_initial[[1]], "data_request")){
      do.call(identify.data_request, dots_initial)
    }else{
      search_terms <- identify(galah_call(), ...)$identify
      return(search_terms)
    }    
  }
}

#' @rdname galah_identify
#' @param .data An object of class `data_request`, created using [request_data()]
#' @importFrom tibble tibble
#' @export
identify.data_request <- function(.data, ...){
  dots_initial <- list(...)
  if (length(dots_initial) < 1) {
    warn("No query passed to `identify()`")
    result <- NULL
  }else{
    if(inherits(dots_initial[[1]], "data.frame") & length(dots_initial) == 1){
      result <- dots_initial[[1]]
      
    }else{
      result <- tibble("search_term" = unlist(dots_initial))
    }
  }
  update_data_request(.data, identify = result)
}

#' @rdname galah_identify
#' @param .data An object of class `metadata_request`, created using [request_metadata()]
#' @export
identify.metadata_request <- function(.data, ...){
  .data <- identify.data_request(.data, ...)
  class(.data) <- "metadata_request"
  if(grepl("-unnest$", .data$type)){
    .data$type <- "taxa-unnest"
  }else{
    .data$type <- "taxa" 
  }
  .data
}

## BELOW HERE IS OLD CODE
# some useful-looking stuff here, but I don't think it's used anywhere rn

#' parser formerly called by `check.R/check_identifiers()`
#' FIXME: check if this is still called
#' @noRd
#' @keywords Internal
parse_identify <- function(input_query, search, call = caller_env()) {
  # get cached behaviour
  atlas <- pour("atlas", "region")
  run_checks <- pour("package", "run_checks")
  verbose <- pour("package", "verbose")
  if (search) {
    lookup <- search_taxa(input_query)
    query <- verify_taxa_ids(lookup, input_query, verbose)
  } else { # i.e. user has passed search = FALSE
    if (atlas == "Australia" && run_checks) {
      lookup <- search_identifiers(input_query)
      query <- verify_taxa_ids(lookup, input_query, verbose, run_checks)
    } else {
      query <- input_query # pass unchanged
    }
  }
  # check_is_character(query) # Q: do we need this function? Is it called elsewhere?
  result <- tibble(identifier = as.character(query))
  return(result)
}

verify_taxa_ids <- function(lookup, input_query, verbose, run_checks, call = caller_env()) {
  if (!any(names(lookup) == "taxon_concept_id")){
    bullets <- c(
      "`galah_identify` didn't return anything.",
      i = "Have you checked whether your search returns the correct taxa?",
      i = "Use `search_taxa` or `search_identifiers` to verify search terms."
    )
    abort(bullets, call = call)
  } else {
    verified_ids <- lookup$taxon_concept_id[!is.na(lookup$taxon_concept_id)]
    if (verbose) {
      # If query is within c(), place each element in separate list
      if(length(input_query[[1]]) > 1) {
        input_query <- as.list(input_query[[1]])
      }
      # calculate number of verified IDs
      n_provided <- length(input_query)
      n_returned <- length(verified_ids)
      check_number_returned(n_provided, n_returned)
    }
    return(verified_ids)
  }
}

#' checker function based on `galah_filter.R/check_filters`
#' @importFrom rlang abort
#' @importFrom rlang have_name
#' @noRd
#' @keywords Internal
check_queries <- function(dots, error_call = caller_env()) {
  if(any(have_name(dots))){
    bullets <- c(
      "We detected a named input.",
      i = glue("This usually means that you've used `=` somewhere."),
      i = glue("`galah_identify` doesn't require equations.")
    )
    abort(bullets, call = error_call)
  }
}

#' Internal function to check number of matched taxa
#' @importFrom rlang warn
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
check_number_returned <- function(n_in, n_out, error_call = caller_env()) {
  if(n_out < n_in){
    bullets <- c(
      "Unmatched taxa.",
      "*" = glue("Results returned for {n_out} of {n_in} taxon IDs.")
    )
    warn(bullets)
  }
}

#' it is possible that the above will lead to non-character 
#' arguments being passed (if search = FALSE and run_checks = FALSE).
#' Internal function to check this
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
check_is_character <- function(query, error_call = caller_env()){
  if(!inherits(query, "character")){
    abort("galah_identify() requires characters to work", call = error_call)
  }
}
