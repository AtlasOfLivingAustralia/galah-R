#' Describe what fields are available
#' 
#' Unlike a local `tibble`, where printing to the console shows what fields
#' are available, when working remotely there is no way to know what fields
#' are present that can be queried. This function name is borrowed from SQL,
#' where it is used to give a read-out of fields that are in the source 
#' database. `r lifecycle::badge("experimental")`
#' @param x An object of class `data_request`
#' @param ... Other arguments, currently ignored
#' @name describe
#' @seealso \code{\link[=glimpse.data_request]{glimpse()}} for a different 
#' way to view results of a query; [show_all_fields()] for full metadata 
#' on available fields; \code{\link[=distinct.data_request]{distinct()}}
#' for showing the values *within* a given field.
#' @order 1
#' @returns A `tibble` showing the `id`, `description` and `data_type`
#' for all fields, or if `select()` is called, then those fields
#' requested by the user. See [galah_select()] for other examples of valid 
#' `group` values.
#' @examples \dontrun{
#' # By default, this shows all fields in the source system
#' galah_call() |>
#'   describe() |>
#'   collect()
#' 
#' # If `select()` is called, only requested fields are shown
#' galah_call() |>
#'   select(group = "basic") |>
#'   describe() |>
#'   collect()
#' }
#' @export
describe <- function(x, ...){
  UseMethod("describe")
}

#' @rdname describe
#' @order 2
#' @export
describe.data_request <- function(x, ...){
  update_request_object(x, describe = TRUE)
}