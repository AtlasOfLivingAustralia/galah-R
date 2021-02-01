#' Build a WKT string from an sf spatial object or verify an existing WKT
#' 
#' Used by \code{\link{ala_occurrences}} to restrict search to an area.
#' Provide either a wkt, or an sf object.
#'
#' @param wkt string: wkt to be verified. WKT strings longer than 10000
#' characters will not be accepted by the ALA- see the vignette for how to
#' work around this.
#' @param sf sf object: area to be converted to wkt
#' @return WKT string representing area provided
#' @export select_locations

select_locations <- function(wkt, sf) {
  if (nargs() > 1) {
    stop("Only one of wkt and sf can be provided to this function")
  }
  if (!missing(wkt)) {
    validate_wkt(wkt)
  }
  if (!missing(sf)) {
    wkt <- build_wkt(sf)
  }
  #attr(wkt, "ala") <- "location"
  return(wkt)
}

# build a valid wkt string from a spatial polygon
build_wkt <- function(polygon) {
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