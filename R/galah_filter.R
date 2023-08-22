#' Narrow a query by specifying filters
#'
#' "Filters" are arguments of the form `field` `logical` `value` that are used
#' to narrow down the number of records returned by a specific query.
#' For example, it is common for users to request records from a particular year
#' (`year == 2020`), or to return all records except for fossils
#'  (`basisOfRecord != "FossilSpecimen"`).
#'  
#' The result of `galah_filter()` can be passed to the `filter`
#' argument in [atlas_occurrences()], [atlas_species()], 
#' [atlas_counts()] or [atlas_media()]. 
#' 
#' `galah_filter` uses non-standard evaluation (NSE),
#' and is designed to be as compatible as possible with `dplyr::filter()`
#' syntax.
#'
#' @param ... filters, in the form `field logical value`
#' @param profile 
#'    `r lifecycle::badge("soft-deprecated")` Use `galah_apply_profile` instead. 
#'    
#'    If supplied, should be a `string` recording a data quality profile to 
#'    apply to the query. See [show_all_profiles()] for valid profiles. By 
#'    default no profile is applied.
#' @return A tibble containing filter values.
#' @seealso [search_taxa()] and [galah_geolocate()] for other ways to restrict 
#' the information returned by [atlas_occurrences()] and related functions. Use
#' `search_all(fields)` to find fields that
#' you can filter by, and [show_values()] to find what values
#' of those filters are available.
#' @details
#' All statements passed to `galah_filter()` (except the `profile`
#' argument) take the form of field - logical - value. Permissible examples include:
#'   * `=` or `==` (e.g. `year = 2020`)
#'   * `!=`, e.g. `year != 2020`)
#'   * `>` or `>=` (e.g. `year >= 2020`)
#'   * `<` or `<=` (e.g. `year <= 2020`)
#'   * `OR` statements (e.g. `year == 2018 | year == 2020`)
#'   * `AND` statements (e.g. `year >= 2000 & year <= 2020`)
#' 
#' In some cases `R` will fail to parse inputs with a single equals sign 
#' (`=`), particularly where statements are separated by `&` or 
#' `|`. This problem can be avoided by using a double-equals (`==`) instead.
#' 
#' *Notes on behaviour*
#' 
#' Separating statements with a comma is equivalent to an `AND` statement; 
#' Ergo `galah_filter(year >= 2010 & year < 2020)` is the same as
#' `galah_filter(year >= 2010, year < 2020)`.
#'     
#' All statements must include the field name; so
#' `galah_filter(year == 2010 | year == 2021)` works, as does 
#' `galah_filter(year == c(2010, 2021))`, but `galah_filter(year == 2010 | 2021)` 
#' fails. 
#'     
#' It is possible to use an object to specify required values, e.g.
#' `year_value <- 2010; galah_filter(year > year_value)`
#'     
#' `solr` supports range queries on text as well as numbers; so this is valid: 
#' `galah_filter(cl22 >= "Tasmania")`
#' 
#' @examples
#' # Filter query results to return records of interest
#' galah_call() |>
#'   galah_filter(year >= 2019,
#'                basisOfRecord == "HumanObservation") |>
#'   atlas_counts()
#' 
#' @importFrom rlang caller_env         
#' @importFrom rlang enquos
#' @importFrom rlang eval_tidy
#' @importFrom rlang have_name
#' @importFrom rlang get_env
#' @importFrom rlang new_quosure
#' @importFrom rlang parse_expr
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_squash
#' @export
galah_filter <- function(..., profile = NULL){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures(dots)
  if(is.null(parsed_dots$data_request)){
    parsed_dots$data
  }else{
    update_data_request(parsed_dots$data_request, 
                      filter = parsed_dots$data)
  }
}

#' @rdname galah_filter
#' @param .data An object of class `data_request`, created using [request_data()]
#' @export
filter.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  update_data_request(.data, 
                      filter = parse_quosures(dots)$data) # see `quosure_handling.R`
}

#' @rdname galah_filter
#' @param .data An object of class `metadata_request`, created using [request_metadata()]
#' @export
filter.metadata_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  .data$filter <- tibble(query = parsed_dots[[1]])
  return(.data)
}

#' @rdname galah_filter
#' @param .data An object of class `files_request`, created using [request_files()]
#' @export
filter.files_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  .data$filter <- parse_quosures(dots)$data  # see `quosure_handling.R`
  return(.data)
}