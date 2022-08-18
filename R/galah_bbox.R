#' Narrow a query to within a bounding box using a spatial object or shapefile
#'
#' Restrict results to be within a bounding box (a box constructed from latitude 
#' & longitude coordinates). Bounding box coordinates can be supplied or 
#' extracted from supplied `sf` objects or shapefiles.
#'
#' @param ... bounding box coordinates supplied as a `data.frame` or `tibble` or 
#' an `sf` object
#' @details `sf` objects will be simplified to their bbox coordinates.
#' @return length-1 object of class `character` and `atlas_locations`,
#' containing a WKT string representing the area provided.
#' @seealso [galah_polygon()] & [galah_geolocate()] for other ways to narrow 
#' queries by location. See [search_taxa()], [galah_filter()] and
#' [galah_select()] for other ways to restrict the information
#' returned by [atlas_occurrences()] and related functions.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Search for records using a shapefile
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(email = "your-email@email.com")
#' 
#' location <- galah_geolocate(st_read(path/to/shapefile))
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_polygon(location) |>
#'   atlas_occurrences()
#' ```
#' 
#' Search for records using a Well-known Text geometry (WKT)
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
#' 
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_polygon(wkt) |>
#'   atlas_counts()
#' ```
#' 
#' @importFrom sf st_cast 
#' @importFrom sf st_as_text 
#' @importFrom sf st_as_sfc
#' @importFrom sf st_is_empty
#' @importFrom sf st_is_simple
#' @importFrom sf st_is_valid
#' @importFrom sf st_geometry type
#' @importFrom rlang try_fetch
#' @importFrom sf st_bbox
#' 
#' @export
galah_bbox <- function(...) {
  
  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")
  checked_dots <- detect_data_request(dots)
  if(!inherits(checked_dots, "quosures")){
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  }else{
    is_data_request <- FALSE
  }
  
  # check that only 1 WKT is supplied at a time
  check_n_inputs(dots)
  
  # convert dots to query
  query <- parse_basic_quosures(dots[1])
  
  # make sure shapefiles are processed correctly
  if (!inherits(query, "sf")) {
    if (!inherits(query, c("tbl", "data.frame", "matrix"))) {
      if (nesting_depth(query) > 1) {
        query <- query[[1]] # FIXME: unlist lists = is this the best way to handle this? Maybe error?
      }
      query <- query[[1]]}} else {query <- query}
  
  # check object is accepted class
  if (!inherits(query, c("list", "matrix", "data.frame", "tbl", "sf", "sfc", "XY"))) {
    unrecognised_class <- class(query)
    bullets <- c(
      "`galah_bbox` input must be an sf object, data.frame or tibble.",
      x = glue("Can't use object of class '{unrecognised_class}'.")
    )
    
    if (inherits(query, "character")) {
      suggest <- c(
        i = "Did you mean to use `galah_polygon`?")
      abort(c(bullets, suggest), call = caller_env())
    } else abort(bullets, call = caller_env()) 
  }
  
  # handle shapefiles
  if (inherits(query, "XY")) query <- sf::st_as_sfc(query) 
  
  # make sure coordinates can be extracted from data.frame/tibble
  # make sure sf objects are valid
  if (!inherits(query, c("sf", "sfc"))) {
    if (inherits(query, c("tbl", "data.frame"))) {
      check_n_rows(query)
      
      # check for informative bbox column names
      valid_col_names <- c("xmin", "xmax", "ymin", "ymax")
      if (!identical(sort(names(query)), sort(valid_col_names))) {
        col_names <- names(query[!names(query) %in% valid_col_names])
        col_names <- glue::glue_collapse(col_names, 
                                         sep = ", ")
        bullets <- c(
          "Can't identify axes of bounding box.",
          i = "`galah_bbox` accepts bbox coordinates from a tibble/data.frame.",
          i = "Column names must be: 'xmin', 'xmax', 'ymin', 'ymax'.",
          x = glue("Unrecognised column name(s): '{col_names}'.")
        )
        abort(bullets, call = caller_env())
      } 
      else {
        query <- st_bbox(c(xmin = query$xmin,
                           xmax = query$xmax,
                           ymin = query$ymin, 
                           ymax = query$ymax),
                         crs = st_crs("WGS84")) 
        bbox_coords <- paste0(tibble(query))
        query <- query |> st_as_sfc()
        inform(glue("
             Data returned for bounding box:
             {bbox_coords}"))
      }
    }
    }
  else {
    # validate sf object
    valid <- query |> st_is_valid()
    if(is.na(valid)) {
      bullets <- c(
        "Invalid spatial object or WKT detected.",
        i = "Check that the spatial feature or WKT in `galah_polygon` is correct."
      )
      abort(bullets, call = caller_env())
    } else {
      query <- query |> st_bbox(crs = st_crs("WGS84")) 
      bbox_coords <- paste0(tibble(query))
      query <- query |> st_as_sfc() # FIXME: should we define the projection?
      inform(glue("
             Data returned for bounding box:
             {bbox_coords}"))
    }
  }
  
  # currently a bug where the ALA doesn't accept some polygons
  # to avoid any issues, any polygons are converted to multipolygons
  if(inherits(query, "sf") || inherits(query, "sfc")) {
    out_query <- build_wkt(query)
  }
  
  attr(out_query, "call") <- "galah_geolocate"
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, geolocate = out_query)
  }else{
    out_query
  }   
}


# build a valid wkt string from a spatial polygon
build_wkt <- function(polygon, error_call = caller_env()) {
  if (st_geometry_type(polygon) == "POLYGON") {
    polygon <- st_cast(polygon, "MULTIPOLYGON")
  }
  if (!st_is_simple(polygon)) {
    bullets <- c(
      "The area provided to `galah_polygon` is too complex. ",
      i = "Try simplifying using `mapview::ms_simplify()` and try again.")
    abort(bullets, call = caller_env())
  }
  wkt <- st_as_text(st_geometry(polygon))
  wkt
}

check_n_rows <- function(tibble, error_call = caller_env()) {
  if (nrow(tibble) > 1) {
    ignored_rows <- paste(2:(nrow(tibble)))
    tibble <- tibble[1,]
    bullets <- c(
      "More than 1 set of coordinates supplied to `galah_bbox`.",
      "*" = glue("Using first row, ignoring row(s) {ignored_rows}.")
    )
    warn(bullets, call = error_call)
  }
}

nesting_depth <- function(object, object_depth=0){
  if(!is.list(object)){
    return(object_depth)
  }else{
    return(
      max(
        unlist(
          lapply(object, nesting_depth, object_depth = object_depth + 1))))    
  }
}
