#' Group by one or more variables
#'
#' Most data operations are done on groups defined by variables. `group_by()`
#' takes a field name (unquoted) and performs a grouping operation. The default
#' behaviour is to use it in combination with 
#' \code{\link[=count.data_request]{count()}} to give information on number 
#' of occurrences per level of that field. Alternatively, you can use it 
#' without count to get a download of occurrences grouped by that variable. This
#' is particularly useful when used with a taxonomic `ID` field (`speciesID`,
#' `genusID` etc.) as it allows further information to be appended to the result.
#' This is how [atlas_species()] works, for example. See 
#' \code{\link[=select.data_request]{select()}} for details.
#' @param .data An object of class `data_request`
#' @param ... Zero or more individual column names to include
#' @return If any arguments are provided, returns a `data.frame` with
#' columns `name` and `type`, as per [select.data_request()].
#' @examples \dontrun{
#' # default usage is for grouping counts
#' galah_call() |> 
#'   group_by(basisOfRecord) |>
#'   counts() |>
#'   collect()
#' 
#' # Alternatively, we can use this with an occurrence search  
#' galah_call() |>
#'   filter(year == 2024,
#'          genus = "Crinia") |>
#'   group_by(speciesID) |>
#'  collect()
#' # note that this example is equivalent to `atlas_species()`; 
#' # but using `group_by()` is more flexible.
#' }
#' @export
group_by.data_request <- function(.data, ...){
parsed_dots <- enquos(..., .ignore_empty = "all") |>
  parse_quosures_basic()
df <- parse_group_by(parsed_dots)
update_data_request(.data, group_by = df)
}

#' @rdname group_by.data_request
#' @importFrom stringr str_detect
#' @export
galah_group_by <- function(...){
  dots <- enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           df <- parse_quosures_basic(dots[-1]) |>
             parse_group_by()
           update_data_request(dots[[1]], group_by = df)
         },
         {
           parse_quosures_basic(dots) |>
             parse_group_by()
         })
}

#' Internal parsing of `group_by` args
#' @noRd
#' @keywords Internal
parse_group_by <- function(dot_names){
  if(length(dot_names) > 0){
    if(length(dot_names) > 3){
      bullets <- c(
        "Too many fields supplied.",
        i = "`group_by.data_request` accepts a maximum of 3 fields."
        )
      abort(bullets, call = caller_env())
    }
    if(length(dot_names) > 0){
      df <- tibble(name = dot_names)
      df$type <- ifelse(str_detect(df$name, "[[:lower:]]"), "field", "assertions")
    }else{
      df <- tibble(name = "name", type = "type", .rows = 0)
    }
  }else{
    df <- tibble(name = "name", type = "type", .rows = 0)
  }
  
  return(df)
}

# for passing to atlas_counts, see rgbif::count_facet
# in practice, the only fields allowable by `path <- /occurrence/counts` 
# are `year` (with optional year range);
# https://api.gbif.org/v1/occurrence/counts/year?year=1981,2012 
# NOTE: range query is optional

# galah_call() |> 
#   galah_group_by(year) |> 
#   galah_filter(year >= 1981 & year <= 2012) |>
#   atlas_counts() 

# ...and `basisOfRecord` (no filters)
# https://api.gbif.org/v1/occurrence/counts/basisOfRecord

# galah_call() |> 
#   galah_group_by(basisOfRecord) |> 
#   atlas_counts()
