#' Specify fields to group when downloading record counts
#'
#' `count.data_request()` and `atlas_counts()` support server-side grouping of 
#' data. Grouping can be used to return record counts grouped by multiple, valid 
#' fields (found by `search_all(fields)`).
#' @param .data An object of class `data_request`
#' @param ... zero or more individual column names to include
#' @param expand `logical`: should factor levels be expanded? Defaults to `TRUE`
#' @return If any arguments are provided, returns a `data.frame` with
#' columns `name` and `type`, as per [select.data_request()].
#' @examples
#' galah_call() |> 
#'   galah_group_by(basisOfRecord) |>
#'   atlas_counts()
#' @export
galah_group_by <- function(..., expand = TRUE){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  df <- parse_group_by(parsed_dots$data, expand)
  if(is.null(parsed_dots$data_request)){
    df
  }else{
    update_data_request(parsed_dots$data_request, group_by = df)
  }
}
 

#' @rdname galah_group_by
#' @export
group_by.data_request <- function(.data, ..., expand = TRUE){
  dots <- enquos(..., .ignore_empty = "all")
  parsed_dots <- parse_quosures_basic(dots)
  df <- parse_group_by(parsed_dots$data, expand)
  update_data_request(.data, group_by = df)
}


#' Internal parsing of `group_by` args
#' @noRd
#' @keywords Internal
parse_group_by <- function(dot_names, expand){
  if(length(dot_names) > 0){
     if(pour("package", "run_checks")){
      validate_fields(dot_names)
     }
    if(length(dot_names) > 3){
      abort("`group_by.data_request` can accept a maximum of three fields")
    }
    
    available_variables <- dot_names[dot_names %in% show_all_fields()$id]
    if(length(available_variables) > 0){
      df <- tibble(name = available_variables)
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
