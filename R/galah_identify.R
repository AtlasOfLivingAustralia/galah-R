#' Narrow a query by passing taxonomic identifiers
#'
#' @description
#' When conducting a search or creating a data query, it is common to identify 
#' a known taxon or group of taxa to narrow down the records or results returned. 
#' `identify()` is used to identify taxa you want returned in a search or 
#' a data query. Users to pass scientific names or taxonomic identifiers
#' with pipes to provide data only for the biological group of interest. 
#' 
#' It is good to use [search_taxa()] and [search_identifiers()] 
#' first to check that the taxa you provide to `galah_identify()` return the 
#' correct results.
#' @name identify.data_request
#' @order 1
#' @param x An object of class `data_request`, created using [request_data()]
#' @param ... One or more scientific names.
#' @param search 
#'   `r lifecycle::badge("deprecated")`  
#'   `galah_identify()` now always does a search to verify search terms; ergo
#'    this argument is ignored.
#' @return A tibble containing identified taxa.
#' @seealso \code{\link[=filter.data_request]{filter()}} or [geolocate()] for 
#' other ways to filter a query. You can also use [search_taxa()] to check that 
#' supplied names are being matched correctly on the server-side; see 
#' [taxonomic_searches] for a detailed overview.
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @examples \dontrun{
#' # Use `galah_identify()` to narrow your queries
#' galah_call() |> 
#'   identify("Eolophus") |>
#'   count() |>
#'   collect()
#' 
#' # If you know a valid taxon identifier, use `filter()` instead.
#' id <- "https://biodiversity.org.au/afd/taxa/009169a9-a916-40ee-866c-669ae0a21c5c"
#' galah_call() |> 
#'   filter(lsid == id)  |>
#'   count() |>
#'   collect()
#' }
#' @importFrom lifecycle deprecate_stop
#' @importFrom lifecycle deprecate_warn
#' @importFrom rlang warn
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

#' @rdname identify.data_request
#' @param x An object of class `metadata_request`, created using [request_metadata()]
#' @order 2
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

#' @rdname identify.data_request
#' @order 3
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