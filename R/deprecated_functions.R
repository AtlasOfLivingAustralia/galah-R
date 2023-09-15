#' Deprecated functions
#'
#' These include:
#' - `galah_down_to()` in favour of `galah_filter()`
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
#' @rdname deprecated-functions
#' @importFrom rlang warn
#' @export
galah_down_to <- function(...){
  
  warn(c("`galah_down_to()` is depreatated",
         i = "Use `filter(rank == chosen_rank)` instead"))
  
  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  rank <- parsed_dots$data
  result <- galah_filter(rank == {{rank}})
  
  # if a data request was supplied, return one
  if(is.null(parsed_dots$data_request)){
    result
  }else{
    update_data_request(parsed_dots$data_request, 
                        filter = result)
  }
}
