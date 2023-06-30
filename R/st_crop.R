#' @rdname galah_polygon
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param y A single `sf` object, WKT string or shapefile
#' @export
st_crop.data_request <- function(x, y, ...){
  update_galah_call(x, geolocate = parse_polygon(y))
}