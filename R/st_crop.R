#' Narrow a query to within a specified polygon
#'
#' Restrict results to those from a specified area. Areas must be polygons. 
#' Polygons must be supplied as an `sf` object, a 'well-known text' (WKT) 
#' string, or a shapefile. Polygons and shapefiles must not be overly complex
#' (i.e. have too many characters or too many vertices) or they will not be
#' accepted in a query to the ALA.
#'  
#' `st_crop` is masked from `sf`, but when piped after [galah_call()], is 
#' functionally  synonymous with [galah_polygon()]
#' 
#' `r lifecycle::badge("experimental")` 
#' @seealso  [galah_polygon()], with which this function is synonymous.
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param y A single `sf` object, WKT string or shapefile
#' @param ... currently ignored
#' @export
st_crop.data_request <- function(x, y, ...){
  update_galah_call(x, geolocate = parse_polygon(y))
}