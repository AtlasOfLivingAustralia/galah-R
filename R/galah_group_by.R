#' Specify fields to group when downloading record counts
#'
#' `atlas_counts` supports server-side grouping of data. Grouping can be 
#' used to return record counts grouped by multiple, valid fields (found by 
#' `search_all(fields)`. Use `galah_group_by` when using the 
#' `group_by` argument of `atlas_counts` to return record counts summed
#' by one or more valid fields.
#' @param ... zero or more individual column names to include
#' @param expand `logical`: When passed to `group_by` argument of 
#' `atlas_counts`, should factor levels be expanded? Defaults to `TRUE`.
#' @return If any arguments are provided, returns a `data.frame` with
#' columns `name` and `type`, as per [galah_select()]; if no arguments
#' are provided, returns `NULL`.
#' @seealso [galah_select()], [galah_filter()] and
#' [galah_geolocate()] for related methods.
#' @examples
#' galah_call() |> 
#'   galah_group_by(basisOfRecord) |>
#'   atlas_counts()
#' @export
galah_group_by <- function(..., expand = TRUE){
  
  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")
  if(length(dots) > 0){
    checked_dots <- detect_data_request(dots)
    if(!inherits(checked_dots, "quosures")){
      is_data_request <- TRUE
      data_request <- checked_dots[[1]]
      dots <- checked_dots[[2]]
    }else{
      is_data_request <- FALSE
    }
  }else{
    is_data_request <- FALSE
  }
  
  df <- parse_group_by(dots)
  attr(df, "expand") <- expand
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, group_by = df)
  }else{
    df
  }
}

parse_group_by <- function(dots){
  if(length(dots) > 0){
    provided_variables <- dequote(unlist(lapply(dots, as_label)))
    if (getOption("galah_config")$package$run_checks){
      validate_fields(provided_variables)
    }
    available_variables <- provided_variables[
      provided_variables %in% show_all_fields()$id]
    if(length(available_variables) > 0){
      df <- tibble(name = available_variables)
      df$type <- ifelse(str_detect(df$name, "[[:lower:]]"), "field", "assertions")
    }else{
      df <- tibble(name = "name", type = "type", .rows = 0)
    }
  }else{
    df <- tibble(name = "name", type = "type", .rows = 0)
  }
  
  # append attributes
  attr(df, "call") <- "galah_group_by"
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
