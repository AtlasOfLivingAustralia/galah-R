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
#' @param x An object of class `data_request` or `metadata_request`.
#' @param ... One or more scientific names.
#' @return A `tibble` containing identified taxa.
#' @seealso \code{\link[=filter.data_request]{filter()}} or [geolocate()] for 
#' other ways to filter a query. You can also use [search_taxa()] to check that 
#' supplied names are being matched correctly on the server-side; see 
#' [taxonomic_searches] for a detailed overview.
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
#' @export
identify.data_request <- function(x, ...){
  dots_initial <- list(...)
  if (length(dots_initial) < 1) {
    cli::cli_warn("No query passed to `identify()`.")
    result <- NULL
  }else{
    if(inherits(dots_initial[[1]], "data.frame") & length(dots_initial) == 1){
      result <- dots_initial[[1]]
      
    }else{
      result <- tibble::tibble("search_term" = unlist(dots_initial))
    }
  }
  update_request_object(x,
                        identify = result)
}

#' @rdname identify.data_request
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
galah_identify <- function(...) {
  dots_initial <- list(...)
  if (length(dots_initial) < 1) {
    cli::cli_warn("No query passed to `identify()`.")
    tibble::tibble("search_term" = character())
  }else{
    if(inherits(dots_initial[[1]], "data_request")){
      do.call(identify.data_request, dots_initial)
    }else{
      search_terms <- identify(galah_call(), ...)$identify
      return(search_terms)
    }
  }
}