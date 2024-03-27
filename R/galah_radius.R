#' @rdname galah_geolocate
#' @export
galah_radius <- function(...){
  # check to see if any of the inputs are a data request
  query <- list(...)
  if(length(query) > 1 & inherits(query[[1]], "data_request")){
    dr <- query[[1]]
    query <- query[-1]
  }else{
    dr <- NULL
  }
  # check that only 1 WKT is supplied at a time
  # check_n_inputs(query)
  # parse
  out_query <- parse_point_radius(query)
  # if a data request was supplied, return one
  if(!is.null(dr)){
    update_data_request(dr, geolocate = out_query)
  }else{
    out_query
  }   
}

#' parser for radius
#' @importFrom rlang try_fetch
#' @importFrom sf st_as_sfc 
#' @importFrom sf st_is_valid 
#' @importFrom sf st_coordinates
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @noRd
#' @keywords Internal
parse_point_radius <- function(..., error_call = caller_env()){
  # browser()
  query <- list(...)[[1]]
  
  if(is.null(query[[1]])) {
    abort("Nothing passed.")
  }
  
  query
  
  # make sure shapefiles are processed correctly
  # if (!inherits(query, "sf")) {query <- query[[1]]} else {query <- query}
  
  # Coords are supplied as an `sfc` point or lat/lon arguments
  if (inherits(query, c("list")) && inherits(query[[1]], c("sf", "sfc"))) {
    
    # make sure shapefiles are handled correctly
    coords <- query[[1]] 
    
    # make sure it's a point
    if (!inherits(coords, c("sfc_POINT", "sfc"))) {
      bullets <- c(
        "You haven't supplied a point's coordinates.",
        i = "Did you supply a polygon or multipolygon?",
        i = "`galah_radius()` accepts sfc_POINT objects."
      )
      abort(bullets, call = error_call)
    }
    
    # extract point coordinate values
    lat = st_coordinates(coords)[1]
    lon = st_coordinates(coords)[2]
    
  } 
  # Coords are supplied as lon/lat arguments
  else { # 
      if(inherits(query, "list") && is.null(query$lat) | is.null(query$lon)) {
        bullets <- c(
          "Missing `lat` or `lon` values.",
          i = "Point coordinates can be specified using `lat` & `lon` arguments, or supplied as an `sfc_POINT`."
          )
        abort(bullets, call = error_call)
      } else {
        query <- query
        
        # extract point coordinate values
        lat = query$lat
        lon = query$lon
      }
    }
  
  
  if(is.null(query$radius)) {
    radius = 10
    bullets <- c(
      "Missing `radius` value.",
      "*" = "Setting radius to 10 km."
    )
    warn(bullets)
  } else {
    radius = query$radius
  }

  
  # check object is accepted class
  if (!any(inherits(c(lat, lon, radius), c("numeric", "double", "integer")))) {

    wrong_classes <- tibble(arg = names(query),
           class = lapply(query, class) |> unlist()
           ) |>
      filter(!class %in% c("numeric", "double", "integer"))
    
    unrecognised_class <- glue_collapse(unique(wrong_classes$class), 
                                          sep = ", ")
    bullets <- c(
      "Invalid class detected.",
      i = "Point coordinates can be specified using `lat` & `lon` arguments, or supplied as an `sfc_POINT`.",
      x = glue("`galah_radius()` does not accept type '{unrecognised_class}'.")
    )
    abort(bullets, call = error_call)
  }
  
  # make sure lat/lon aren't impossible
  if(lon > 180 | lon < -180 | lat > 90 | lat < -90) {
    bullets <- c(
      "Point location outside of possible range.",
      i = "Are your point coordinates valid?"
    )
    abort(bullets, call = error_call)
  }
  
  # check that radius is within a certain size?
  
  out_query <- list(lat = lat, lon = lon, radius = radius)
  out_query
}