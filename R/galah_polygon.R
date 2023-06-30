#' Narrow a query to within a specified polygon
#'
#' Restrict results to those from a specified area. Areas must be polygons. 
#' Polygons must be supplied as an `sf` object, a 'well-known text' (WKT) 
#' string, or a shapefile. Polygons and shapefiles must not be overly complex
#' (i.e. have too many characters or too many vertices) or they will not be
#' accepted in a query to the ALA.
#'
#' @rdname galah_polygon
#' @param ... When supplied to `galah_polygon()`, a single `sf` object, WKT 
#' string or shapefile. Currently ignored when supplied to 
#' `st_crop.data_request()`
#' @details WKT strings longer than 10000 characters and 
#' `sf` objects with more than 500 vertices will not be
#' accepted by the ALA. Some polygons  may need to be simplified.
#' @return length-1 object of class `character` and `galah_geolocate`,
#' containing a multipolygon WKT string representing the area provided.
#' @seealso [galah_bbox()] & [galah_geolocate()] for other ways to narrow
#' queries by location. [search_taxa()], [galah_filter()] and
#' [galah_select()] for other ways to restrict the information
#' returned by [atlas_occurrences()] and related functions.
#' 
#' @examples
#' \dontrun{
#' # Search for records within a polygon using an `sf` object
#' location <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))" |>
#'   sf::st_as_sfc()
#' galah_call() |>
#'   galah_identify("reptilia") |>
#'   galah_polygon(location) |>
#'   atlas_counts()
#'
#' # Search for records using a shapefile
#' galah_config(email = "your_email_here")
#' location <- galah_geolocate(sf::st_read(path/to/shapefile.shp))
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_polygon(location) |>
#'   atlas_occurrences()
#' 
#' # Search for records using a Well-known Text string (WKT)
#' wkt <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))"
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_polygon(wkt) |>
#'   atlas_counts()
#' }
#' @importFrom sf st_cast st_as_text st_as_sfc st_is_empty st_is_simple
#' @importFrom sf st_crs st_geometry st_geometry_type st_is_valid st_simplify st_read
#' @importFrom rlang try_fetch
#' @importFrom assertthat assert_that is.string
#' 
#' @keywords internal
#' 
#' @export
galah_polygon <- function(...) {
  
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
  
  # parse
  out_query <- parse_polygon(query)
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, geolocate = out_query)
  }else{
    out_query
  }   
}


parse_polygon <- function(query){

  # make sure shapefiles are processed correctly
  if (!inherits(query, "sf")) {query <- query[[1]]} else {query <- query}
  
  # check object is accepted class
  if (!inherits(query, c("character", "list", "matrix", "data.frame", "tbl", "sf", "sfc", "XY"))) {
    
    unrecognised_class <- class(query)
    bullets <- c(
      "Invalid object detected.",
      i = "Did you provide WKT in the right format?",
      x = glue("`galah_polygon` cannot use object of class '{unrecognised_class}'.")
    )
    abort(bullets, call = caller_env())
  }
  
  # handle shapefiles
  if (inherits(query, "XY")) query <- sf::st_as_sfc(query) 
  
  # make sure spatial object or wkt is valid
  if (!inherits(query, c("sf", "sfc"))) {
    check_wkt_length(query)
    
    # handle errors from converting impossible WKTs
    query <- rlang::try_fetch(
      query |> st_as_sfc(), 
      error = function(cnd) {
        bullets <- c(
          "Invalid WKT detected.",
          i = "Check that the spatial feature or WKT in `galah_polygon` is correct."
        )
        abort(bullets, call = caller_env())
      })
    
    # validate that wkt/spatial object is real
    valid <- query |> st_is_valid() }
  else {
    valid <- query |> st_is_valid()
  }
  if(is.na(valid)) {
    bullets <- c(
      "Invalid spatial object or WKT detected.",
      i = "Check that the spatial feature or WKT in `galah_polygon` is correct."
    )
    abort(bullets, call = caller_env())
  }
  
  # check number of vertices of WKT
  if(n_points(query) > 500) {
    n_verts <- n_points(query)
    bullets <- c(
      glue("Polygon must have 500 or fewer vertices, not {n_verts}."),
      i = "`galah_polygon` only returns a query for simple polygons.",
      i = "See `?sf::st_simplify` for how to simplify geospatial objects."
    )
    abort(bullets, call = caller_env())
  }
  
  # currently a bug where the ALA doesn't accept some polygons
  # to avoid any issues, any polygons are converted to multipolygons
  if(inherits(query, "sf") || inherits(query, "sfc")) {
    out_query <- build_wkt(query)
  } else {
    
    # remove space after "POLYGON" if present
    if(str_detect(query, "POLYGON \\(\\("))
      query <- str_replace(query, "POLYGON \\(\\(", "POLYGON\\(\\(")
    
    if (str_detect(query, "POLYGON") & ! str_detect(query, "MULTIPOLYGON")) {
      # change start of string
      query <- str_replace(query, "POLYGON\\(\\(", "MULTIPOLYGON\\(\\(\\(")
      # add an extra bracket
      query <- paste0(query, ")")
    }
    out_query <- query
  }
  attr(out_query, "call") <- "galah_geolocate"
  out_query
}


n_points <- function(x) {
  count_vertices(sf::st_geometry(x))
}

# count number of vertices
count_vertices <- function(wkt_string, error_call = caller_env()) {
  out <- if (is.list(wkt_string)) 
    sapply(sapply(wkt_string, count_vertices), sum) 
  else {
    if (is.matrix(wkt_string))
        nrow(wkt_string)
    else {
      if (!sf::st_is_empty(wkt_string)) 1 else
        0
      }
  }
  unname(out)
}

# build a valid wkt string from a spatial polygon
build_wkt <- function(polygon, error_call = caller_env()) {
  if (st_geometry_type(polygon) == "POLYGON") {
    polygon <- st_cast(polygon, "MULTIPOLYGON")
  }
  if (!st_is_simple(polygon)) {
    bullets <- c(
      "The area provided to `galah_polygon` is too complex. ",
      i = "See `?sf::st_simplify` for how to simplify geospatial objects.")
    abort(bullets, call = caller_env())
  }
  wkt <- st_as_text(st_geometry(polygon))
  wkt
}

check_wkt_length <- function(wkt, error_call = caller_env()) {
  if (is.string(wkt) == TRUE |
    is.matrix(wkt) == TRUE  | 
    is.list(wkt) == TRUE | 
    is.data.frame(wkt) == TRUE) {
    # make sure strings aren't too long for API call
    assert_that(is.string(wkt))
    n_char_wkt <- nchar(wkt)
    max_char <- 10000
    if (n_char_wkt > max_char) {
      bullets <- c(
        "Invalid WKT detected.",
        x = glue("WKT string can be maximum {max_char} characters. WKT supplied has {n_char_wkt}.")
      )
      abort(bullets, call = error_call)
    }
  } 

  # check that first and last point match if object is a polygon
  # FIXME: Do we need this to work anymore? Or does st_is_valid() solve this?
  # else {
  #   sf_obj <- st_as_sfc(wkt)
  #   if (st_geometry_type(sf_obj) == "POLYGON") {
  #     first_coord <- trimws(str_split(str_split(wkt, "\\(\\(")[[1]][2], ",")[[1]][1])
  #     last_coord <- gsub("\\)\\)", "",trimws(tail(str_split(wkt, ",")[[1]], n = 1)))
  #     if (isFALSE(first_coord == last_coord)) {
  #       warn("The first and last coordinates of the polygon provided may not be the same.")
  #     }
  #   }
  # }
}