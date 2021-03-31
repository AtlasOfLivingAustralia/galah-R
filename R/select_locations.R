#' Build a WKT string from an sf spatial object or verify an existing WKT
#'
#' Restrict results to those from a specified area. Areas must be polygons
#' and be specified as either an sf object, or a 'well-known text' (wkt) string.
#'
#' @param sf sf object: area to be converted to wkt
#' @param wkt string: wkt to be verified. WKT strings longer than 10000
#' characters will not be accepted by the ALA- see the vignette for how to
#' work around this.
#' @return WKT string representing the area provided.
#' @seealso \code{\link{select_taxa}}, \code{\link{select_filters}} and
#' \code{\link{select_columns}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions.
#' @examples \dontrun{
#' # Search for records using a shapefile
#' locations <- select_locations(sf = path/to/shapefile)
#' ala_occurrences(locations = locations)
#' 
#' # Search for records using a WKT
#' wkt <- "POLYGON((142.36228 -29.00703,142.74131 -29.00703,142.74131 \
#' -29.39064,142.36228 -29.39064,142.36228 -29.00703))"
#' ala_occurrences(wkt = select_locations(wkt = wkt))
#' }
#' 
#' @export select_locations

select_locations <- function(sf, wkt) {
  # currently a bug where the ALA doesn't accept some polygons
  # to avoid any issues, any polygons should be converted to multipolygons
  if (nargs() > 1) {
    stop("Only one of wkt and sf can be provided to this function")
  }
  if (!missing(wkt)) {
    validate_wkt(wkt)
    if (str_detect(wkt, "POLYGON") & ! str_detect(wkt, "MULTIPOLYGON")) {
      # replace start of string
      wkt <- str_replace(wkt, "POLYGON\\(\\(", "MULTIPOLYGON\\(\\(\\(")
      # add an extra bracket
      wkt <- paste0(wkt, ")")
    }
  }
  if (!missing(sf)) {
    wkt <- build_wkt(sf)
  }

  #attr(wkt, "ala") <- "location"
  return(wkt)
}

# build a valid wkt string from a spatial polygon
build_wkt <- function(polygon) {
  if (st_geometry_type(polygon) == "POLYGON") {
    polygon <- st_cast(polygon, "MULTIPOLYGON")
  }
  wkt <- st_as_text(st_geometry(polygon))
  if (nchar(wkt) > 10000) {
    stop("The area provided is too complex. Please simplify it and try again.")
  }
  wkt
}

validate_wkt <- function(wkt) {
  max_char <- 10000
  if (nchar(wkt) > max_char) {
    stop("The WKT string provided is greater than ", max_char,
         " characters , please simplify and try again.")
  }
  else if (!wellknown::lint(wkt)) {
    warning("The WKT provided may be invalid.")
  }
  # check that first and last point of match if object is a polygon
  else {
    sf_obj <- st_as_sfc(wkt)
    if (st_geometry_type(sf_obj) == "POLYGON") {
      first_coord <- trimws(str_split(str_split(wkt, "\\(\\(")[[1]][2], ",")[[1]][1])
      last_coord <- gsub("\\)\\)", "",trimws(tail(str_split(wkt, ",")[[1]], n = 1)))
      if (isFALSE(first_coord == last_coord)) {
        warning("The first and last coordinates of the polygon provided may not be the same.")
      }
    }
  }
}