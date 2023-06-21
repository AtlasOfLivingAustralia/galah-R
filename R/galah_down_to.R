#' Specify the lowest taxonomic rank required in a downwards search
#'
#' `atlas_taxonomy` generates a downwards search of the taxonomic tree. Use 
#' `galah_down_to()` to specify the taxonomic level to search to. 
#' `galah_down_to()` uses non-standard evaluation (NSE).
#' 
#' @param ... the name of a single taxonomic rank
#' @return A string with the named rank
#' @seealso [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for related methods.
#' 
#' @examples
#' # Return a taxonomic tree of *Chordata* down to the class level
#' \dontrun{
#' galah_call() |>
#'     galah_identify("Vertebrata") |>
#'     galah_down_to(class) |>
#'     atlas_taxonomy()
#' }
#' @export

galah_down_to <- function(...){
  
  if(missing(...)){
    bullets <- c(
      "Argument `rank` is missing, with no default.",
      i = "Did you forget to specify a taxonomic level?",
      i = "See `?galah_down_to` for more information."
    )
    abort(bullets, call = caller_env())
  }
  
  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  rank <- parsed_dots$data
  
  # repeat error for empty dots
  if(length(rank) < 1){
    bullets <- c(
      "Argument `rank` is missing, with no default.",
      i = "Did you forget to specify a taxonomic level?",
      i = "See `?galah_down_to` for more information."
    )
    abort(bullets, call = caller_env())
  }
  
  # error check for multiple ranks
  n_down_to <- length(rank)
  if(n_down_to > 1){
    bullets <- c(
      "Can't provide tree more than one taxonomic rank to end with.",
      i = "galah_down_to` only accepts a single rank at a time.",
      x = glue("`galah_down_to` has length of {n_down_to}.")
    )
    abort(bullets, call = caller_env())
  }

  # create tibble containing specified rank
  if(rank %in% show_all_ranks()$name){
    result <- tibble(rank = rank)
    attr(result, "call") <- "galah_down_to"
  }else{
    bullets <- c(
      "Invalid taxonomic rank.",
      i = "The rank provided to `galah_down_to` must be a valid taxonomic rank.",
      x = glue("{rank} is not a valid rank.")
    )
    abort(bullets, call = caller_env())
  }

  # if a data request was supplied, return one
  if(is.null(parsed_dots$data_request)){
    result
  }else{
    update_galah_call(parsed_dots$data_request, down_to = result)
  }
}