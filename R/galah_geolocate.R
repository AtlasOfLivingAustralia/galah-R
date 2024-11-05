#' Narrow a query to within a specified area
#'
#' @description
#' Restrict results to those from a specified area.  
#' Areas can be specified as either polygons or bounding boxes, depending on 
#' `type`. Alternatively, users can call the underlying functions directly via
#' `galah_polygon()`, `galah_bbox()` or `galah_radius()`. It is possible to use 
#' `sf` syntax by calling `st_crop()`, which is synonymous with 
#' `galah_polygon()`.
#'  
#' **Use a polygon**
#' If calling `galah_geolocate()`, the default `type` is `"polygon"`, which 
#' narrows queries to within an area supplied as a `POLYGON` or `MULTIPOLYGON`. 
#' Polygons must be 
#' specified as either an `sf` object, a 'well-known text' (WKT) string, or a 
#' shapefile. Shapefiles must be simple to be accepted by the ALA. 
#' 
#' **Use a bounding box**
#' Alternatively, set `type = "bbox"` to narrow queries to within a bounding 
#' box. Bounding boxes can be extracted from a supplied `sf` object or 
#' a shapefile. A bounding box can also be supplied as a `bbox` object 
#' (via `sf::st_bbox()`) or a `tibble`/`data.frame`. 
#'
#' `r lifecycle::badge("experimental")`
#' **Use a point radius**
#' Alternatively, set `type = "radius"` to narrow queries to within a circular 
#' area around a specific point location. Point coordinates can be supplied as 
#' latitude/longitude coordinate numbers or as an `sf` object (`sfc_POINT`).
#' Area is supplied as a `radius` in kilometres. Default radius is 10 km.
#'
#' @param type `string`: one of `c("polygon", "bbox")`. Defaults to
#' `"polygon"`. If `type = "polygon"`, a multipolygon will be built via 
#' [galah_polygon()]. If `type = "bbox"`, a multipolygon will be built via 
#' [galah_bbox()]. The multipolygon is used to narrow a query to the ALA.
#' @param ... For `st_crop`, additional arguments (currently ignored). Otherwise
#' a single `sf` object, WKT string or shapefile. Bounding boxes can
#' be supplied as a `tibble`/`data.frame` or a `bbox`
#' @details 
#' If `type = "polygon"`, WKT strings longer than 10000 characters and 
#' `sf` objects with more than 500 vertices will not be
#' accepted by the ALA. Some polygons  may need to be simplified.
#' If `type = "bbox"`, sf objects and shapefiles will be converted to a bounding 
#' box to query the ALA. 
#' If `type = "radius`, `sfc_POINT` objects will be converted to lon/lat 
#' coordinate numbers to query the ALA. Default radius is 10 km.
#' @return 
#' If `type = "polygon"` or `type = "bbox"`, 
#' length-1 string (class `character`) containing a multipolygon WKT 
#' string representing the area provided. 
#' If `type = "radius"`,
#' `list` of `lat`, `long` and `radius` values.
#' @name geolocate
#' @order 1
#' @examples \dontrun{
#' # Search for records within a polygon using a shapefile
#' location <- sf::st_read("path/to/shapefile.shp")
#' galah_call() |>
#'   identify("vulpes") |>
#'   geolocate(location) |>
#'   count() |>
#'   collect()
#'   
#' # Search for records within the bounding box of a shapefile
#' location <- sf::st_read("path/to/shapefile.shp")
#' galah_call() |>
#'   identify("vulpes") |>
#'   geolocate(location, type = "bbox") |>
#'   count() |>
#'   collect()
#' 
#' # Search for records within a polygon using an `sf` object
#' location <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))" |>
#'  sf::st_as_sfc()
#' galah_call() |>
#'   identify("reptilia") |>
#'   galah_polygon(location) |>
#'   count() |>
#'   collect()
#'   
#' # Search for records using a Well-known Text string (WKT)
#' wkt <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))"
#' galah_call() |>
#'   identify("vulpes") |>
#'   st_crop(wkt) |>
#'   count() |>
#'   collect()
#' 
#' # Search for records within the bounding box extracted from an `sf` object
#' location <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))" |>
#'   sf::st_as_sfc()
#' galah_call() |>
#'   identify("vulpes") |>
#'   galah_geolocate(location, type = "bbox") |>
#'   count() |>
#'   collect()
#' 
#' # Search for records using a bounding box of coordinates
#' b_box <- sf::st_bbox(c(xmin = 143, xmax = 148, ymin = -29, ymax = -28), 
#'                      crs = sf::st_crs("WGS84"))
#' galah_call() |>
#'   identify("reptilia") |>
#'   galah_geolocate(b_box, type = "bbox") |>
#'   count() |>
#'   collect()
#'
#' # Search for records using a bounding box in a `tibble` or `data.frame`
#' b_box <- tibble::tibble(xmin = 148, ymin = -29, xmax = 143, ymax = -21)
#' galah_call() |>
#'   identify("vulpes") |>
#'   galah_geolocate(b_box, type = "bbox") |>
#'   count() |>
#'   collect()
#' 
#' # Search for records within a radius around a point's coordinates
#' galah_call() |>
#'   identify("manorina melanocephala") |>
#'   galah_geolocate(lat = -33.7,
#'                   lon = 151.3,
#'                   radius = 5,
#'                   type = "radius") |>
#'   count() |>
#'   collect()
#' 
#' # Search for records with a radius around an `sf_POINT` object
#' point <- sf::st_sfc(sf::st_point(c(-33.66741, 151.3174)), crs = 4326)
#' galah_call() |>
#'   identify("manorina melanocephala") |>
#'   galah_geolocate(point,
#'                   radius = 5,
#'                   type = "radius") |>
#'   count() |>
#'   collect()
#' }
#' @export
geolocate <- function(..., type = c("polygon", "bbox", "radius")) {
  
  if (is.null(type)) type = "polygon"
  
  type <- match.arg(type)
  switch(type, 
         polygon = galah_polygon(...),
         bbox = galah_bbox(...),
         radius = galah_radius(...)
  )
}

#' @rdname geolocate
#' @order 2
#' @export
galah_geolocate <- geolocate

#' @rdname geolocate
#' @order 6
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param y A valid Well-Known Text string (wkt), a `POLYGON` or a `MULTIPOLYGON`
#' @export
st_crop.data_request <- function(x, y, ...){
  update_data_request(x, geolocate = parse_polygon(y))
}