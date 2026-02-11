#' Unnest a query
#' 
#' This syntax is borrowed from `tidyr`, and is conceptually used in the same
#' way here, but in galah unnest amends the query to unnest information
#' server-side, rather than on your machine. It powers all of the 
#' [show_values()] functions in galah.
#' @details
#' Re-implementing existing functions has the consequence of supporting 
#' consistent syntax with tidyverse, at the cost of potentially introducing 
#' conflicts. This can be avoided by using the `::` operator where required.
#' @param .query An object of class `metadata_request`
#' @returns An object of class `metadata_request`
#' @examples \dontrun{
#' # Return values of field `basisOfRecord`
#' request_metadata() |> 
#'   unnest() |> 
#'   filter(field == basisOfRecord) |> 
#'   collect()
#'   
#' # Using `galah::unnest()` in this way is equivalent to:
#' show_all(fields, "basisOfRecord") |> 
#'   show_values()
#'   
#' # to add information to a species list:
#' request_metadata() |>
#'   filter(list == "dr650") |>
#'   select(everything()) |>
#'   unnest() |>
#'   collect()
#' }
#' @export
unnest <- function(.query){
  if(!inherits(.query, "metadata_request")){
    cli::cli_abort("`galah::unnest()` can only be used with objects of class `metadata_request`.")
  }
  if(!is.null(.query$filter)){
    # check whether `type` is supplied as singular (i.e. `field` not `fields`)
    supplied_type <- .query$filter$variable[1]
    if(supplied_type != "taxa" & !grepl("s$", supplied_type)){
      supplied_type <- glue::glue("{supplied_type}s")
    }
  }else if(!is.null(.query$identify)){
    supplied_type <- "taxa"
  }else{
    supplied_type <- .query$type
  }
  # ensure only used with certain query types
  valid_types <- c("fields", "lists", "profiles", "taxa")
  if(!(supplied_type %in% valid_types)){
    c("Invalid `type` supplied to `unnest()`",
      i = "Valid types are `fields`, `lists`, `profiles` or `taxa`") |>
      cli::cli_abort()
  }
  .query$type <- glue::glue("{supplied_type}-unnest")
  .query
}
