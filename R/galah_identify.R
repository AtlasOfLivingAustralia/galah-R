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
#' @param ... One or more scientific names.
#' @param search 
#'   `r lifecycle::badge("deprecated")`  
#'   `galah_identify()` now always does a search to verify search terms; ergo
#'    this argument is ignored.
#' @return A tibble containing identified taxa.
#' @seealso [search_taxa()] to find identifiers from scientific names;
#' [search_identifiers()] for how to get names if taxonomic identifiers 
#' are already known.
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @examples \dontrun{
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
#' # Within a pipe, `identify()` and `galah_identify()` are synonymous.
#' # hence the following is identical to the previous example:
#' request_data() |>
#'   identify("Eolophus") |>
#'   count() |>
#'   collect()
#' 
#' # If you know a valid taxon identifier, use `galah_filter()` instead.
#' # (This was formerly supported by `galah_identify()` with `search = FALSE`)
#' id <- "https://biodiversity.org.au/afd/taxa/009169a9-a916-40ee-866c-669ae0a21c5c"
#' galah_call() |> 
#'   galah_filter(lsid == id) |>
#'   atlas_counts()
#' }
#' @importFrom lifecycle deprecate_stop
#' @importFrom lifecycle deprecate_warn
#' @importFrom rlang warn
#' @export
galah_identify <- function(..., search = NULL) {
  dots_initial <- list(...)
  if (length(dots_initial) < 1) {
    warn("No query passed to `identify()`.")
    tibble("search_term" = character())
  }else{
    dots_initial <- check_search_arg(dots_initial, search)
    if(inherits(dots_initial[[1]], "data_request")){
      do.call(identify.data_request, dots_initial)
    }else{
      search_terms <- identify(galah_call(), ...)$identify
      return(search_terms)
    }
  }
}

#' @rdname galah_identify
#' @param x An object of class `data_request`, created using [request_data()]
#' @importFrom tibble tibble
#' @export
identify.data_request <- function(x, ...){
  dots_initial <- list(...)
  if (length(dots_initial) < 1) {
    warn("No query passed to `identify()`.")
    result <- NULL
  }else{
    if(inherits(dots_initial[[1]], "data.frame") & length(dots_initial) == 1){
      result <- dots_initial[[1]]
      
    }else{
      result <- tibble("search_term" = unlist(dots_initial))
    }
  }
  update_data_request(x, identify = result)
}

#' @rdname galah_identify
#' @param x An object of class `metadata_request`, created using [request_metadata()]
#' @export
identify.metadata_request <- function(x, ...){
  x <- identify.data_request(x, ...)
  class(x) <- "metadata_request"
  if(grepl("-unnest$", x$type)){
    x$type <- "taxa-unnest"
  }else{
    x$type <- "taxa" 
  }
  x
}

#' Remove `search` argument from `galah_identify()`, give deprecated warning
#' @importFrom lifecycle deprecate_warn
#' @noRd
#' @keywords Internal
check_search_arg <- function(dots_initial, search = NULL) {
  if("search" %in% names(dots_initial)) {
    search <- dots_initial$name
    dots_initial <- dots_initial[names(dots_initial) != "search"]
  } 
  if(!is.null(search)){
    if(!is.logical(search)){
      deprecate_stop(
        when = "2.0.0",
        what = "galah_identify(search = )",
        details = glue("`galah_identify()` now always does a search to verify search terms. \\
                   Passing anything other than TRUE or FALSE to `search` has never worked"))      
    }else{
      if(search){
        # if search = TRUE, this function still behaves correctly
        deprecate_warn(
          when = "2.0.0",
          what = "galah_identify(search = )",
          details = glue("`galah_identify()` now always does a search to verify search terms. \\
                   Please remove `search` argument from `galah_identify()`."))      
        # if search = FALSE, abort warning to use filter(lsid == x) instead
      }else{
        deprecate_stop(
          when = "2.0.0",
          what = "galah_identify(search = )",
          details = glue("`galah_identify()` now always does a search to verify search terms. \\
                   To pass identifiers, please use `filter(lsid == 'identifier_here') instead."))
      }     
    }
  }
  dots_initial
}