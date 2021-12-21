#' Specify the lowest taxonomic rank required in a downwards search
#'
#' `atlas_taxonomy` generates a downwards search of the taxonomic tree. This
#' function can be used to specify the name of a valid taxonomic rank using
#' non-standard evaluation (NSE), for consistency with other `galah_` functions.
#' @param ... the name of a single taxonoimc rank
#' @return A string with the named rank
#' @seealso [galah_select()], [galah_filter()] and
#' [galah_location()] for related methods.
#' @examples
#' # A basic case
#' atlas_taxonomy(
#'     taxa = search_taxa("Chordata")
#'     down_to = galah_down_to(class)
#'     )
#' @importFrom dplyr filter  
#' @export

galah_down_to <- function(...){
  
  dots <- as.list(match.call(expand.dots = FALSE)$...)
  if(missing(...)){
    stop("`galah_down_to` requires a taxonomic rank")
  }
  if(length(dots) > 1){
    stop("`galah_down_to` only accepts a single taxonomic rank")
  }
  
  result <- filter(show_all_ranks(), name == dots[[1]])$name
  
  if(length(result) < 1){
    stop(paste(dots[[1]], "is not a valid taxonomic rank"))
  }else{
    class(result) <- append(class(result), "galah_down_to")
    result
  }
}