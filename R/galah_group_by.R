#' Specify fields to group when downloading record counts
#'
#' `atlas_counts` supports server-side grouping of data. Grouping can be 
#' used to return record counts grouped by multiple, valid fields (found by 
#' `search_fields`. Use `galah_group_by` when using the 
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
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Return record counts since 2010 by year
#' 
#' ```{r, comment = "#>", collapse = TRUE, results = "hide"}
#' records <- galah_call() |> 
#'     galah_filter(year > 2010) |>
#'     galah_group_by(year) |>
#'     atlas_counts()
#' ```
#' ```{r, comment = "#>", collapse = TRUE}
#' records
#' ```
#'  
#' Return record counts since 2010 by year and data provider
#' 
#' ```{r, comment = "#>", collapse = TRUE, results = "hide"}
#' records <- galah_call() |>
#'     galah_filter(year > 2010) |>
#'     galah_group_by(year, dataResourceName) |>
#'     atlas_counts()
#' ```
#' ```{r, comment = "#>", collapse = TRUE}
#' records
#' ```
#'     
#' Return record counts of *Litoria* species each year since 2015, limiting
#' results to the top 5 each year
#' 
#' ```{r, comment = "#>", collapse = TRUE, results = "hide"}
#' records <- galah_call() |>
#'     galah_identify("Litoria") |>
#'     galah_filter(year > 2015) |>
#'     galah_group_by(year, species) |>
#'     atlas_counts(limit = 5)
#' ```
#' ```{r, comment = "#>", collapse = TRUE}
#' records
#' ```
#' 
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
  
  # if there are any arguments provided, parse them
  if(length(dots) > 0){
    provided_variables <- dequote(unlist(lapply(dots, as_label)))
    if (getOption("galah_config")$run_checks){
      validate_fields(provided_variables)
    } 
    available_variables <- provided_variables[provided_variables %in% show_all_fields()$id]
    if(length(available_variables) > 0){
      df <- tibble(name = available_variables)
      df$type <- ifelse(str_detect(df$name, "[[:lower:]]"), "field", "assertions")
      class(df) <- append(class(df), "galah_group_by")
      attr(df, "expand") <- expand
    }else{
      df <- tibble(name = "name", type = "type", .rows = 0)
      df <- set_galah_object_class(df, new_class = "galah_group_by")
      attr(df, "expand") <- expand
      df
    }
  }else{
    df <- tibble(name = "name", type = "type", .rows = 0)
    df <- set_galah_object_class(df, new_class = "galah_group_by")
    attr(df, "expand") <- expand
    df
  }
 
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, group_by = df)
  }else{
    df
  }
}