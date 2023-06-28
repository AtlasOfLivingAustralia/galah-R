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
#' \dontrun{
#' # Return a taxonomic tree of *Chordata* down to the class level
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
  if(length(dots) > 0){
    checked_dots <- detect_data_request(dots)
    if(!inherits(checked_dots, "quosures")){
      is_data_request <- TRUE
      data_request <- checked_dots[[1]]
      dots <- checked_dots[[2]]
    }else{
      is_data_request <- FALSE
    }
  }else{
    is_data_request <- FALSE
  }
  
  # repeat error for empty dots
  if(length(dots) < 1){
    bullets <- c(
      "Argument `rank` is missing, with no default.",
      i = "Did you forget to specify a taxonomic level?",
      i = "See `?galah_down_to` for more information."
    )
    abort(bullets, call = caller_env())
  }
  
  # error check for multiple ranks
  if(length(dots) > 1){
    n_down_to <- length(dots)
    bullets <- c(
      "Can't provide tree more than one taxonomic rank to end with.",
      i = "galah_down_to` only accepts a single rank at a time.",
      x = glue("`galah_down_to` has length of {n_down_to}.")
    )
    abort(bullets, call = caller_env())
  }

  # create tibble containing specified rank
  rank <- dequote(unlist(lapply(dots, function(a){deparse(quo_squash(a))})))
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
  if(is_data_request){
    update_galah_call(data_request, down_to = result)
  }else{
    result
  }
}