#' Keep rows that match a condition
#'
#' The `filter()` function is used to subset a data, retaining all rows that 
#' satisfy your conditions. To be retained, the row must produce a value of 
#' `TRUE` for all conditions. Unlike 'local' filters that act on a `tibble`,
#' the galah implementations work by amending a query which is then enacted 
#' by `collect()` or one of the `atlas_` family of functions (such as 
#' `atlas_counts()` or `atlas_occurrences()`).
#' @name filter.data_request
#' @order 1
#' @param .data An object of class `data_request`, `metadata_request` 
#' or `files_request`, created using [galah_call()] or related functions.
#' @param ... Expressions that return a logical value, and are defined in terms 
#' of the variables in the selected atlas (and checked using `show_all(fields)`. 
#' If multiple expressions are included, they are combined with the & operator. 
#' Only rows for which all conditions evaluate to `TRUE` are kept.
#' @param profile 
#'    `r lifecycle::badge("deprecated")`  
#'    Use `galah_apply_profile` instead. 
#' @return A tibble containing filter values.
#' @seealso \code{\link[=select.data_request]{select()}}, 
#' \code{\link[=group_by.data_request]{group_by()}} and [geolocate()] for 
#' other ways to amend the information returned by [atlas_()] functions. Use 
#' `search_all(fields)` to find fields that you can filter by, and 
#' [show_values()] to find what values of those filters are available.
#' @details
#' 
#' *Syntax*
#' 
#' `filter.data_request()` and `galah_filter()` uses non-standard evaluation 
#' (NSE), and are designed to be as compatible as possible with 
#' `dplyr::filter()` syntax. Permissible examples include:
#' 
#'   * `==` (e.g. `year = 2020`) but not `=` (for consistency with `dplyr`)
#'   * `!=`, e.g. `year != 2020`)
#'   * `>` or `>=` (e.g. `year >= 2020`)
#'   * `<` or `<=` (e.g. `year <= 2020`)
#'   * `OR` statements (e.g. `year == 2018 | year == 2020`)
#'   * `AND` statements (e.g. `year >= 2000 & year <= 2020`)
#' 
#' Some general tips:
#'  * Separating statements with a comma is equivalent to an `AND` statement; 
#' Ergo `filter(year >= 2010 & year < 2020)` is the same as
#' `_filter(year >= 2010, year < 2020)`.
#'  * All statements must include the field name; so
#' `filter(year == 2010 | year == 2021)` works, as does 
#' `filter(year == c(2010, 2021))`, but `filter(year == 2010 | 2021)` 
#' fails. 
#'  * It is possible to use an object to specify required values, e.g.
#' `year_value <- 2010; filter(year > year_value)`.
#'  * `solr` supports range queries on text as well as numbers; so 
#' `filter(cl22 >= "Tasmania")` is valid.
#'  * It is possible to filter by 'assertions', which are statements about data 
#' validity, such as `filter(assertions != c("INVALID_SCIENTIFIC_NAME", "COORDINATE_INVALID")`.
#' Valid assertions can be found using `show_all(assertions)`.
#' 
#' *Exceptions*
#' 
#' When querying occurrences, species, or their respective counts (i.e. all of
#' the above examples), field names are checked internally against 
#' `show_all(fields)`. There are some cases where bespoke field names are 
#' required, as follows.
#' 
#' When requesting a data download from a DOI, the field `doi` is valid, i.e.:
#' \preformatted{galah_call() |> 
#'   filter(doi = "a-long-doi-string") |> 
#'   collect()}
#' 
#' For taxonomic metadata, the `taxa` field is valid:
#' \preformatted{request_metadata() |> 
#'   filter(taxa == "Chordata") |> 
#'   unnest()}
#'  
#' For building taxonomic trees, the `rank` field is valid:
#' \preformatted{request_data() |>
#'   identify("Chordata") |>
#'   filter(rank == "class") |>
#'   atlas_taxonomy()}
#'   
#' Media queries are more involved, but break two rules: they accept the `media`
#' field, and they accept a tibble on the rhs of the equation. For example, 
#' users wishing to break down media queries into their respective API calls
#' should begin with an occurrence query:
#' 
#' \preformatted{occurrences <- galah_call() |> 
#'    identify("Litoria peronii) |> 
#'    select(group = c("basic", "media") |> 
#'    collect()}
#' 
#' They can then use the `media` field to request media metadata:
#' \preformatted{media_metadata <- galah_call("metadata") |>
#'   filter(media == occurrences) |>
#'   collect()}
#' 
#' And finally, the metadata tibble can be used to request files:
#' \preformatted{galah_call("files") |>
#'   filter(media == media_metadata) |>
#'   collect()}
#'   
#' @examples \dontrun{
#' galah_call() |>
#'   filter(year >= 2019,
#'          basisOfRecord == "HumanObservation") |>
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
filter.data_request <- function(.data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  check_named_input(dots)
  update_data_request(.data, 
                      filter = parse_quosures_data(dots)) # see `quosure_handling.R`
}
# usually filters as previously for ALA, but some exceptions:
# doi == "x" in `atlas_occurrences()`
# rank == "class" in `atlas_taxonomy()` replacement for `galah_down_to()`

#' @rdname filter.data_request
#' @order 2
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

#' @rdname filter.data_request
#' @order 3
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

#' @rdname filter.data_request
#' @order 4
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