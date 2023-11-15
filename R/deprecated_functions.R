#' Deprecated functions
#'
#' These include:
#' - `galah_down_to()` in favour of `galah_filter()`
#' @param ... the name of a single taxonomic rank
#' @return A string with the named rank
#' @seealso [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for related methods.
#' 
#' @examples \dontrun{
#' # Return a taxonomic tree of *Chordata* down to the class level
#' galah_call() |>
#'     galah_identify("Vertebrata") |>
#'     galah_down_to(class) |>
#'     atlas_taxonomy()
#' }
#' @rdname deprecated-functions
#' @importFrom lifecycle deprecate_warn
#' @export
galah_down_to <- function(...){
  
  deprecate_warn(when = "2.0.0",
                            what = "galah_down_to()",
                            details = "Use `filter(rank == \"chosen_rank\")` instead."
                            )
  
  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           rank <- parse_quosures_basic(dots[-1])
           dots[[1]] |> filter.data_request(rank == {{rank}})
         },
         {
           rank <- parse_quosures_basic(dots)
           result <- galah_call() |> 
             filter(rank == {{rank}})
           result$filter
         })
}
