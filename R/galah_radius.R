#' @rdname geolocate
#' @order 5
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
#' @importFrom sf st_coordinates
#' @importFrom stringr str_detect
#' @noRd
#' @keywords Internal
parse_point_radius <- function(..., error_call = caller_env()){
  
  query <- try_fetch(
    list(...)[[1]],
    error = function(cnd) {
      bullets <- c(
        "No input detected.",
        i = "Did you forget to supply coordinates to `galah_radius()`?"
      )
      abort(bullets, call = error_call)
  })
  
  # Coords are supplied as an `sfc` point or lat/lon arguments
  if (inherits(query, c("list")) && inherits(query[[1]], c("sf", "sfc"))) {
    
    # make sure shapefiles are handled correctly
    coords <- query[[1]] 
    
    # make sure it's a point
    if (!inherits(coords, c("sfc_POINT"))) {
      
      # TODO: Recognise when more than one point is passed, default to first point
      
      unrecognised_class <- glue::glue_collapse(class(coords), sep = ", ")
      bullets <- c(
        "Invalid spatial object supplied as point coordinates.",
        i = "`galah_radius()` accepts sfc_POINT objects.",
        x = glue("Cannot use class: {unrecognised_class}.")
      )
      abort(bullets, call = error_call)
    }
    
    # extract point coordinate values
    lat = st_coordinates(coords)[1]
    lon = st_coordinates(coords)[2]
    
  } 
  # Coords are supplied as lon/lat arguments
  else { # 
    
    # TODO: Assign numeric values to lon/lat automatically?
    # if(!is.null(query[[1]]) && inherits(query[[1]], c("numeric", "double", "integer")) |
    #    !is.null(query[[2]]) && inherits(query[[2]], c("numeric", "double", "integer"))) {
    #   lon = query[[1]]
    #   lat = query[[2]]
    # }
      if(inherits(query, "list") && is.null(query$lat) | is.null(query$lon)) {
        bullets <- c(
          "Missing `lat` or `lon` values.",
          i = "Point coordinates should be specified using `lat` & `lon` arguments, or supplied as an `sfc_POINT`."
          )
        abort(bullets, call = error_call)
      } else {
        query <- query
        
        # extract point coordinate values
        lat = query$lat
        lon = query$lon
      }
    }
  
  # Only use first set of coordinates
  if(length(lon) > 1 | length(lat) > 1 ) {
    check_n_inputs(lon)
    check_n_inputs(lat)
    lat <- lat[[1]]
    lon <- lon[[1]]
  }
  
  # Check for radius value. If empty, set to 10 km
  if(is.null(query$radius)) {
    bullets <- c(
      "No radius value specified.",
      "*" = "Setting radius to 10 km."
    )
    warn(bullets)
    radius = 10
  } else {
    # Only use one radius value
    if(length(query$radius) > 1) {
      n_radius <- length(query$radius)
      warn(c("More than 1 radius provided.",
             "*" = glue("Using first radius, ignoring additional {n_radius - 1} value(s)."))
      )
      radius <- query$radius[[1]]
    } else {
      radius = query$radius
    }
  }
  
  # Check object is accepted class when supplied to lat/lon/radius arguments
  if (!any(inherits(c(lat, lon, radius), c("numeric", "double", "integer")))) {

    wrong_classes <- tibble(arg = names(query),
           class = lapply(query, class) |> unlist()
           ) |>
      filter(!class %in% c("numeric", "double", "integer"))
    
    unrecognised_class <- glue_collapse(unique(wrong_classes$class), 
                                          sep = ", ")
    bullets <- c(
      "Invalid class detected.",
      i = "Point can be specified as numeric `lat` & `lon` coordinates, or supplied as an `sfc_POINT`.",
      x = glue("`galah_radius()` does not accept type '{unrecognised_class}'.")
    )
    abort(bullets, call = error_call)
  }
  
  # make sure lat/lon aren't impossible
  if(lon > 180 | lon < -180 | lat > 90 | lat < -90) {
    bullets <- c(
      "Point location outside of possible range.",
      i = "Are the coordinates valid?"
    )
    abort(bullets, call = error_call)
  }
  
  # Should this be an error? A message?
  if(radius > 1565) {
    bullets <- c(
      "Radius is larger than the area of Australia.",
      i = "Try reducing the radius to narrow your query."
    )
    inform(bullets)
  }
  
  out_query <- list(lat = lat, 
                    lon = lon, 
                    radius = radius)
  out_query
}