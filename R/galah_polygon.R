#' Narrow a query using a spatial object, shapefile or WKT
#'
#' Restrict results to those from a specified area. Areas must be polygons
#' and be specified as either an sf object, or a 'well-known text' (WKT) string.
#'
#' @param ... a single WKT string or sf object
#' @details WKT strings longer than 10000 characters will not be
#' accepted by the ALA - so the sf object or WKT string may need to be
#' simplified.
#' @return length-1 object of class `character` and `atlas_locations`,
#' containing a WKT string representing the area provided.
#' @seealso [search_taxa()], [galah_filter()] and
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
  if(length(dots) > 1){
    n_geolocations <- length(dots)
    bullets <- c(
      "More than 1 spatial area provided to `galah_polygon`.",
      "*" = glue("Using first location only, ignoring additional {n_geolocations - 1} location(s).")
    )
    warn(bullets, call = caller_env())
  }

  # convert dots to query
  query <- parse_basic_quosures(dots[1])
  
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
  
  if (inherits(query, "XY")) query <- sf::st_as_sfc(query) # handle shapefiles
  
  # make sure spatial object or wkt is valid
  if (!inherits(query, c("sf", "sfc"))) {
    validate_wkt(query)
    query <- query |> st_as_sfc()
    valid <- query |> st_is_valid() }
    else {
    valid <- query |> st_is_valid()
    }
  if(is.na(valid)) {
      bullets <- c(
        "Invalid spatial object or WKT.",
        i = "Check that the spatial feature or WKT you entered is correct."
      )
      abort(bullets, call = caller_env())
  }
  
  # check number of vertices of WKT
  # if(n_points(query) > 8) {
  #   n_verts <- n_points(query)
  #   bullets <- c(
  #     "WKT object is too complex.",
  #     i = "`galah_polygon` only returns a query for simple polygons.",
  #     x = glue("Polygon must have 8 or fewer vertices, not {n_verts}.")
  #   )
  #   abort(bullets, call = caller_env())
  # }
  
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
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, geolocate = out_query)
  }else{
    out_query
  }   
  
  
  
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
    abort("The area provided it too complex. Please simplify using mapview::ms_simplify() and try again.", 
          call = error_call)
  }
  wkt <- st_as_text(st_geometry(polygon))
  
  
  # if (nchar(wkt) > 10000) {
  #   abort("The area provided is too complex. Please simplify it and try again.",
  #         call = error_call)
  # }
  
  wkt
}

validate_wkt <- function(wkt, error_call = caller_env()) {
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

  # check that first and last point of match if object is a polygon
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
