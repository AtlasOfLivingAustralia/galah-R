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
#'    `r lifecycle::badge("deprecated")`  
#'    Use `galah_apply_profile` instead. 
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
#' It is possible to filter by 'assertions', which are statements about data 
#' validity, e.g. to remove those lacking critical spatial or taxonomic data:
#' `galah_filter(assertions != c("INVALID_SCIENTIFIC_NAME", "COORDINATE_INVALID")`
#' Valid assertions can be found using `show_all(assertions)`.
#' 
#' @examples \dontrun{
#' # Filter query results to return records of interest
#' galah_call() |>
#'   galah_filter(year >= 2019,
#'                basisOfRecord == "HumanObservation") |>
#'   atlas_counts()
#' 
#' # Alternatively, the same call using `dplyr` functions:
#' request_data() |>
#'   filter(year >= 2019,
#'                basisOfRecord == "HumanObservation") |>
#'   count() |>
#'   collect()
#' }
#' @importFrom rlang caller_env         
#' @importFrom rlang enquos
#' @importFrom rlang eval_tidy
#' @importFrom rlang get_env
#' @importFrom rlang new_quosure
#' @importFrom rlang parse_expr
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_squash
#' @export
galah_filter <- function(..., profile = NULL){
  dots <- enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  check_named_input(dots)
  switch(class(dots[[1]])[1],
         "data_request" = {
           update_data_request(dots[[1]], 
                               filter = parse_quosures_data(dots[-1]))
         },
         "metadata_request" = {
           parse_quosures_metadata(dots[[1]], dots[-1])
         },
         "files_request" = {
           input <- dots[[1]]
           parsed_dots <- parse_quosures_files(dots[-1])
           input$filter <- parsed_dots$data
           input$type <- parsed_dots$variable
           input
         },
         parse_quosures_data(dots)
  )
}

#' @rdname galah_filter
#' @param .data An object of class `data_request`, created using [request_data()]
#' @export
filter.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  check_named_input(dots)
  update_data_request(.data, 
                      filter = parse_quosures_data(dots)) # see `quosure_handling.R`
}
# usually filters as previously for ALA, but some exceptions:
  # doi == "x" in `atlas_occurrences()`
  # rank == "class" in `atlas_taxonomy()` replacement for `galah_down_to()`

#' @rdname galah_filter
#' @param .data An object of class `metadata_request`, created using [request_metadata()]
#' @export
filter.metadata_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  check_named_input(dots)
  parse_quosures_metadata(.data, dots)
}
# Note: the intended purpose of this function is to pass `filter()`
# within the API call in the same was as `filter.data_request()`. 
# In theory this would power `search_all()`; but in practice many 
# APIs do not support a `q` argument that allows server-side filtering.
# The exception is GBIF.
# 
# An unusual distinction is that when `unnest()` is also called, `filter()` is 
# used to set the thing that is unnested; this is a different kind of search
# e.g. `request_metadata() |> filter(taxa == "Chordata") |> unnest()`

#' simple parser for metadata
#' @noRd
#' @keywords Internal
parse_quosures_metadata <- function(request, dots){
  dots_parsed <- parse_quosures_files(dots)
  request$filter <- dots_parsed
  # The `filter` argument sets `type` when specified
  initial_type <- request$type
  supplied_type <- dots_parsed$variable[1]
  if(!(supplied_type %in% c("taxa", "media")) &
     !grepl("s$", supplied_type)){
    filter_type <- paste0(supplied_type, "s")
  }else{
    filter_type <- supplied_type
  }
  if(grepl("-unnest$", initial_type)){
    request$type <- paste0(filter_type, "-unnest")
  }else{
    request$type <- filter_type
  }
  request
}

#' @rdname galah_filter
#' @param .data An object of class `files_request`, created using [request_files()]
#' @importFrom rlang .data
#' @export
filter.files_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  check_named_input(dots)
  dots_parsed <- parse_quosures_files(dots)
  check_files_filter(dots_parsed)
  .data$type <- dots_parsed$variable
  .data$filter <- dots_parsed$data
  .data
}
