#' Specify the lowest taxonomic rank required in a downwards search
#'
#' `atlas_taxonomy` generates a downwards search of the taxonomic tree. This
#' function can be used to specify the name of a valid taxonomic rank using
#' non-standard evaluation (NSE), for consistency with other `galah_` functions.
#' @param ... the name of a single taxonomic rank
#' @return A string with the named rank
#' @seealso [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for related methods.
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
    bullets <- c(
      "Argument `down_to` is missing, with no default.",
      i = "Did you forget to specify a taxonomic level?",
      i = "See `?galah_down_to` for more information."
    )
    abort(bullets, call = caller_env())
  }
  if(length(dots) > 1){
    n_down_to <- length(dots)
    bullets <- c(
      "Can't provide tree more than one taxon to end with.",
      i = "galah_down_to` only accepts a single taxon at a time.",
      x = glue("`galah_down_to` has length of {n_down_to}.")
    )
    abort(bullets, call = caller_env())
  }
  
  result <- filter(show_all_ranks(), name == dots[[1]])$name
  
  if(length(result) < 1){
    rank <- paste(dots[[1]])
    bullets <- c(
      "Invalid taxonomic rank.",
      i = "The rank provided to `galah_down_to` must be a valid taxonomic rank.",
      x = glue("{rank} is not a valid rank.")
    )
    abort(bullets, call = caller_env())
  }else{
    class(result) <- append(class(result), "galah_down_to")
    result
  }
}