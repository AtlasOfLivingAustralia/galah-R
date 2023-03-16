#' Narrow a query to within a specified area
#'
#' Restrict results to those from a specified area. Areas can be specified as 
#' either polygons or bounding boxes, depending on `type`.
#' 
#' By default, `type` is set to `"polygon"` which narrows queries to within an area 
#' supplied as a `POLYGON`. Polygons must be specified as either an `sf` object, 
#' a 'well-known text' (WKT) string, or a shapefile. Shapefiles must be simple 
#' to be accepted by the ALA. 
#' 
#' Alternatively, set `type = "bbox"` to narrow queries to within a 
#' bounding box. Bounding boxes can be extracted from a supplied `sf` object or 
#' a shapefile. A bounding box can also be supplied as a `bbox` object 
#' (via `sf::st_bbox()`) or a `tibble`/`data.frame`. 
#'
#' @param ... a single `sf` object, WKT string or shapefile. Bounding boxes can
#' be supplied as a `tibble`/`data.frame` or a `bbox`
#' @param type `string`: one of `c("polygon", "bbox")`. Defaults to
#' `"polygon"`. If `type = "polygon"`, a multipolygon will be built via 
#' [galah_polygon()]. If `type = "bbox"`, a multipolygon will be built via 
#' [galah_bbox()]. The multipolygon is used to narrow a query to the ALA.
#' @details If `type = "polygon"`, WKT strings longer than 10000 characters and 
#' `sf` objects with more than 500 vertices will not be
#' accepted by the ALA. Some polygons  may need to be simplified. If 
#' `type = "bbox"`, sf objects and shapefiles will be converted to a bounding 
#' box to query the ALA.
#' @return length-1 object of class `character` and `galah_geolocate`,
#' containing a multipolygon WKT string representing the area provided.
#' @seealso [galah_polygon()] and [galah_bbox()] for specific functions to 
#' narrow queries by a specified area. [search_taxa()], [galah_filter()] and
#' [galah_select()] for other ways to restrict the information
#' returned by [atlas_occurrences()] and related functions.
#' 
#' @examples
#' \dontrun{
#' # Search for records within a polygon using a shapefile
#' location <- sf::st_read("path/to/shapefile.shp")
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_geolocate(location) |>
#'   atlas_counts()
#'   
#' # Search for records within the bounding box of a shapefile
#' location <- sf::st_read("path/to/shapefile.shp")
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_geolocate(location, type = "bbox") |>
#'   atlas_counts()
#' 
#' # Search for records within a polygon using an `sf` object
#' location <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))" |>
#'  sf::st_as_sfc()
#' galah_call() |>
#'   galah_identify("reptilia") |>
#'   galah_polygon(location) |>
#'   atlas_counts()
#'    
#' # Search for records using a Well-known Text string (WKT)
#' wkt <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))"
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_geolocate(wkt) |>
#'   atlas_counts()
#' 
#' # Search for records within the bounding box extracted from an `sf` object
#' location <- "POLYGON((142.3 -29.0,142.7 -29.1,142.7 -29.4,142.3 -29.0))" |>
#'   sf::st_as_sfc()
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_geolocate(location, type = "bbox") |>
#'   atlas_counts()
#' 
#' # Search for records using a bounding box of coordinates
#' b_box <- sf::st_bbox(c(xmin = 143, xmax = 148, ymin = -29, ymax = -28), 
#'                      crs = sf::st_crs("WGS84"))
#' galah_call() |>
#'   galah_identify("reptilia") |>
#'   galah_geolocate(b_box, type = "bbox") |>
#'   atlas_counts()
#'
#' # Search for records using a bounding box in a `tibble` or `data.frame`
#' b_box <- tibble::tibble(xmin = 148, ymin = -29, xmax = 143, ymax = -21)
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_geolocate(b_box, type = "bbox") |>
#'   atlas_counts()
#' }
#' 
#' @export
galah_geolocate <- function(..., type = c("polygon", "bbox")) {
  
  if (is.null(type)) type = "polygon"
  
  type <- match.arg(type)
  switch(type, 
         polygon = galah_polygon(...),
         bbox = galah_bbox(...)
  )

}

