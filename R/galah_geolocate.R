#' @rdname st_crop.data_request
#' @order 2
#' @param type `string`: one of `c("polygon", "bbox")`. Defaults to
#' `"polygon"`. If `type = "polygon"`, a multipolygon will be built via 
#' [galah_polygon()]. If `type = "bbox"`, a multipolygon will be built via 
#' [galah_bbox()]. The multipolygon is used to narrow a query to the ALA.
#' @export
galah_geolocate <- function(..., type = c("polygon", "bbox", "radius")) {
  
  if (is.null(type)) type = "polygon"
  
  type <- match.arg(type)
  switch(type, 
         polygon = galah_polygon(...),
         bbox = galah_bbox(...),
         radius = galah_radius(...)
  )
}