#' @title Keep or drop columns using their names
#'
#' @description Select (and optionally rename) variables in a data frame, using 
#' a concise mini-language that makes it easy to refer to variables based on 
#' their name. Note that unlike calling `select()` on a local tibble, this 
#' implementation is only evaluated at the 
#' \code{\link[=collapse.data_request]{collapse()}} stage, meaning any errors 
#' or messages will be triggered at the end of the pipe.
#' 
#' `select()` supports `dplyr` **selection helpers**, including:
#' 
#'   * \code{\link[dplyr]{everything}}: Matches all variables. This is treated
#'     unusually in `galah`; see `details`.
#'   * \code{\link[dplyr]{last_col}}: Select last variable, possibly with an 
#'     offset.
#'
#' Other helpers select variables by matching patterns in their names:
#'  
#'   * \code{\link[dplyr]{starts_with}}: Starts with a prefix.
#'   * \code{\link[dplyr]{ends_with}}: Ends with a suffix.
#'   * \code{\link[dplyr]{contains}}: Contains a literal string.
#'   * \code{\link[dplyr]{matches}}: Matches a regular expression.
#'   * \code{\link[dplyr]{num_range}}: Matches a numerical range like x01, 
#'     x02, x03.
#'
#' Or from variables stored in a character vector:
#'  
#'   * \code{\link[dplyr]{all_of}}: Matches variable names in a character 
#'     vector. All names must be present, otherwise an out-of-bounds error is 
#'     thrown.
#'   * \code{\link[dplyr]{any_of}}: Same as `all_of()`, except that no error 
#'     is thrown for names that don't exist.
#'
#' Or using a predicate function:
#'
#'   * \code{\link[dplyr]{where}}: Applies a function to all variables and selects those for which the function returns `TRUE`.
#' @name select.data_request
#' @param .data An object of class `data_request`, created using [galah_call()].
#' @param ... Zero or more individual column names to include.
#' @param group `string`: (optional) name of one or more column groups to
#' include. Valid options are `"basic"`, `"event"` `"taxonomy"`, `"media"` and
#' `"assertions"`.
#' @return A tibble
#' specifying the name and type of each column to include in the 
#' call to `atlas_counts()` or `atlas_occurrences()`.
#' @details
#' GBIF nodes store content in hundreds of different fields, and users often 
#' require thousands or millions of records at a time. To reduce time taken to 
#' download data, and limit complexity of the resulting `tibble`, it is sensible 
#' to restrict the fields returned by occurrence queries. The full list of 
#' available fields can be viewed with `show_all(fields)`. Note that `select()` 
#' and `galah_select()` are supported for all atlases that allow downloads, with 
#' the exception of GBIF, for which all columns are returned.
#' 
#' Calling the argument `group = "basic"` returns the following columns:
#'
#'   * `recordID`
#'   * `scientificName`
#'   * `taxonConceptID`
#'   * `decimalLatitude`
#'   * `decimalLongitude`
#'   * `eventDate`
#'   * `basisOfRecord`
#'   * `occurrenceStatus`
#'   * `dataResourceName`
#' 
#' Using `group = "event"` returns the following columns:
#' 
#'   * `eventRemarks`
#'   * `eventTime`
#'   * `eventID`
#'   * `eventDate`
#'   * `samplingEffort`
#'   * `samplingProtocol`
#' 
#' Using `group = "media"` returns the following columns:
#' 
#'   * `multimedia`
#'   * `multimediaLicence`
#'   * `images`
#'   * `videos`
#'   * `sounds`
#' 
#' Using `group = "taxonomy"` returns higher taxonomic information for a given
#' query. It is the only `group` that is accepted by `atlas_species()` as well 
#' as `atlas_occurrences()`.
#' 
#' Using `group = "assertions"` returns all quality assertion-related
#' columns. The list of assertions is shown by `show_all_assertions()`.
#' 
#' For `atlas_occurrences()`, arguments passed to `...` should be valid field
#' names, which you can check using `show_all(fields)`. For `atlas_species()`,
#' it should be one or more of:
#' 
#'   * `counts` to include counts of occurrences per species.
#'   * `synonyms` to include any synonymous names.
#'   * `lists` to include authoritative lists that each species is included on.
#'
#' For metadata queries - as generated using [request_metadata()] or 
#' [galah_call()] - `select()` can now be used to return only the requested
#' columns. Unlike data queries, this works by capturing the user's query
#' and applying it user-side, rather than amending the query.
#'      
#' @seealso \code{\link[=filter.data_request]{filter()}}, 
#' \code{\link[=st_crop.data_request]{st_crop()}} and
#' \code{\link[=identify.data_request]{identify()}} for other ways to restrict 
#' the information returned; `show_all(fields)` to list available fields.
#' @examples \dontrun{
#' # Download occurrence records of *Perameles*, 
#' # Only return scientificName and eventDate columns
#' galah_config(email = "your-email@email.com")
#' galah_call() |>
#'   identify("perameles")|>
#'   select(scientificName, eventDate) |>
#'   collect()
#' 
#' # Only return the "basic" group of columns and the basisOfRecord column
#' galah_call() |>
#'   identify("perameles") |>
#'   select(basisOfRecord, group = "basic") |>
#'   collect()
#'   
#' # When used in a pipe, `galah_select()` and `select()` are synonymous.
#' # Hence the previous example can be rewritten as:
#' galah_call() |>
#'   galah_identify("perameles") |>
#'   galah_select(basisOfRecord, group = "basic") |>
#'   collect()
#' }
#' @export
select.data_request <- function(.data, ..., group = NULL){
  # if(is_gbif()){
  #  cli::cli_text("`select()` is not supported for GBIF occurrence downloads API v1: skipping")
  #  .data
  # }else{
    dots <- rlang::enquos(..., .ignore_empty = "all")
    list(quosure = dots,
         summary = generate_summary(dots)) |>
      add_group(group) |>
      update_request_object(.data, select = _)  
  # }
}

#' @rdname select.data_request
#' @export
select.metadata_request <- function(.data, ...){
  dots <- rlang::enquos(...,
                        .ignore_empty = "all")
  list(quosure = dots,
       summary = generate_summary(dots)) |>
    update_request_object(.data, select = _)
}


#' internal function to summarise select function (to support `print()`)
#' @noRd
#' @keywords Internal
generate_summary <- function(dots){
  labels <- purrr::map(dots, rlang::expr_text) |>
    unlist() |>
    glue::glue_collapse(sep = " | ")
  labels[labels != "<dat_rqst>"]
}

#' internal function to add `group` arg to the end of a list
#' @noRd
#' @keywords Internal
add_group <- function(dots, group){
  group <- check_groups(group, n = length(dots$quosure))
  summary_length <- nchar(dots$summary)
  if(is.null(group)){
    if(summary_length < 1){
      group <- "basic"
      dots$group <- group
    }else{
      dots$group <- vector(mode = "character", length = 0L) 
    }
  }else{
    dots$group <- group
  }
  if(length(dots$group) > 0){
    if(summary_length < 1){
      separator <- ""
    }else{
      separator <- " | "
    }
    dots$summary <- paste0(dots$summary,
                           separator,
                           "group = ", 
                           paste(group, collapse = ", ")) 
  }
  dots
}