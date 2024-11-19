#' Non-generic tidyverse functions
#' 
#' Several useful functions from tidyverse packages are `generic`, meaning
#' that we can define class-specific versions of those functions and implement
#' them in galah; examples include `filter()`, `select()` and `group_by()`.
#' However, there are also functions that are only defined within tidyverse 
#' packages and are not generic. In a few cases we have re-implemented these 
#' functions in galah. This has the consequence of supporting consistent
#' syntax with tidyverse, at the cost of potentially introducing conflicts.
#' This can be avoided by using the `::` operator where required (see examples).
#' 
#' The following functions are included:
#'  - `desc()` (`dplyr`): Use within `arrange()` to specify arrangement should be descending
#'  - `unnest()` (`tidyr`): Use to 'drill down' into nested information on `fields`, `lists`, `profiles`, or `taxa`  
#'
#' These galah versions all use lazy evaluation.
#' @returns 
#'  - `galah::desc()` returns a `tibble` used by `arrange.data_request()` to arrange rows of a query. 
#'  - `galah::unnest()` returns an object of class `metadata_request`.
#' @seealso \code{\link[=arrange.data_request]{arrange()}}, [galah_call()]
#' @examples \dontrun{
#' # Arrange grouped record counts by descending year
#' galah_call() |>
#'   identify("perameles") |>
#'   filter(year > 2019) |>
#'   count() |>
#'   arrange(galah::desc(year)) |>
#'   collect()
#' 
#' # Return values of field `basisOfRecord`
#' request_metadata() |> 
#'   galah::unnest() |> 
#'   filter(field == basisOfRecord) |> 
#'   collect()
#'   
#' # Using `galah::unnest()` in this way is equivalent to:
#' show_all(fields, "basisOfRecord") |> 
#'   show_values()
#' }
#' @name tidyverse_functions
NULL

# @rdname tidyverse_functions
# @export
# between <- function(){}

#' @rdname tidyverse_functions
#' @param ... column to order by
#' @export
desc <- function(...){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  tibble(variable = parsed_dots,
         direction = "descending")
} 

#' @rdname tidyverse_functions
#' @param .query An object of class `metadata_request`
#' @export
unnest <- function(.query){
  if(!inherits(.query, "metadata_request")){
    abort("`galah::unnest()` can only be used with objects of class `metadata_request`.")
  }
  if(!is.null(.query$filter)){
    supplied_type <- .query$filter$variable[1]
    if(supplied_type != "taxa" &
       !grepl("s$", supplied_type)){
      supplied_type <- paste0(supplied_type, "s")
    }
  }else if(!is.null(.query$identify)){
    supplied_type <- "taxa"
  }else{
    supplied_type <- .query$type
  }
  valid_types <- c("fields", "lists", "profiles", "taxa")
  if(!(supplied_type %in% valid_types)){
    bullets <- c(
      "Invalid `type` supplied to `unnest()`",
      i = "Valid types are `fields`, `lists`, `profiles` or `taxa`")
    abort(bullets, call = caller_env())
  }
  .query$type <- paste0(supplied_type, "-unnest")
  .query
}
