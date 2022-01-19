#' Narrow a query using a WKT string
#'
#' Restrict results to those from a specified area. Areas must be polygons
#' and be specified as either an sf object, or a 'well-known text' (wkt) string.
#'
#' @param ... a single wkt string or sf object
#' @details WKT strings longer than 10000 characters will not be
#' accepted by the ALA- so the sf object or WKT string may need to be
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
#' atlas_occurrences(geolocate = location)
#' ```
#' 
#' Search for records using a Well-known Text geometry (WKT)
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
#' 
#' atlas_counts(geolocate = galah_geolocate(wkt))
#' ```
#' 
#' @export
galah_geolocate <- function(...) {
  
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
  
  # error check for multiple ranks
  if(length(dots) > 1){
    n_geolocations <- length(dots)
    bullets <- c(
      "Can't provide more than one spatial area.",
      i = "`galah_geolocate` only accepts a single area at a time.",
      x = glue("`galah_geolocate` has length of {n_geolocations}.")
    )
    abort(bullets, call = caller_env())
  }
  
  # convert dots to query
  query <- parse_basic_quosures(dots)
    
  # currently a bug where the ALA doesn't accept some polygons
  # to avoid any issues, any polygons should be converted to multipolygons
  if(inherits(query, "sf") || inherits(query, "sfc")) {
    out_query <- build_wkt(query)
  } else {
    validate_wkt(query)
    if (str_detect(query, "POLYGON") & ! str_detect(query, "MULTIPOLYGON")) {
      # replace start of string
      query <- str_replace(query, "POLYGON\\(\\(", "MULTIPOLYGON\\(\\(\\(")
      # add an extra bracket
      query <- paste0(query, ")")
    }
    out_query <- query
  }
  class(out_query) <- append(class(out_query), "galah_geolocate")
  
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
  wkt <- st_as_text(st_geometry(polygon))
  if (nchar(wkt) > 10000) {
    abort("The area provided is too complex. Please simplify it and try again.",
          call = error_call)
  }
  wkt
}

validate_wkt <- function(wkt, error_call = caller_env()) {
  assert_that(is.string(wkt))
  max_char <- 10000
  if (nchar(wkt) > max_char) {
    bullets <- c(
      "Invalid WKT detected.",
      i = "The WKT string provided may be too big.",
      x = "WKT string greater than {max_char} characters."
    )
    abort(bullets, call = error_call)
  }
  else if (!wellknown::lint(wkt)) {
    warn("The WKT provided may be invalid.")
  }
  # check that first and last point of match if object is a polygon
  else {
    sf_obj <- st_as_sfc(wkt)
    if (st_geometry_type(sf_obj) == "POLYGON") {
      first_coord <- trimws(str_split(str_split(wkt, "\\(\\(")[[1]][2], ",")[[1]][1])
      last_coord <- gsub("\\)\\)", "",trimws(tail(str_split(wkt, ",")[[1]], n = 1)))
      if (isFALSE(first_coord == last_coord)) {
        warn("The first and last coordinates of the polygon provided may not be the same.")
      }
    }
  }
}
