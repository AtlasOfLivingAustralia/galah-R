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
#' @noRd
#' @keywords Internal
parse_point_radius <- function(..., error_call = caller_env()){
  
  query <- rlang::try_fetch(
    list(...)[[1]],
    error = function(cnd) {
      c("No input detected.",
        i = "Did you forget to supply coordinates to `galah_radius()`?") |>
      cli::cli_abort(call = error_call)
  })
  
  # Coords are supplied as an `sfc` point or lat/lon arguments
  if (inherits(query, c("list")) && 
      inherits(query[[1]], c("sf", "sfc"))) {
    
    # make sure shapefiles are handled correctly
    coords <- query[[1]] 
    
    # make sure it's a point
    if (!inherits(coords, c("sfc_POINT"))) {
      
      # TODO: Recognise when more than one point is passed, default to first point
      
      unrecognised_class <- glue::glue_collapse(class(coords), sep = ", ")
      c("Invalid spatial object supplied as point coordinates.",
        i = "`galah_radius()` accepts sfc_POINT objects.",
        x = "Cannot use class: {unrecognised_class}.") |>
      cli::cli_abort(call = error_call)
    }
    
    # extract point coordinate values
    lat <- sf::st_coordinates(coords)[1]
    lon <- sf::st_coordinates(coords)[2]
    
  } else { # Coords are supplied as lon/lat arguments
    
    # TODO: Assign numeric values to lon/lat automatically?
    # if(!is.null(query[[1]]) && inherits(query[[1]], c("numeric", "double", "integer")) |
    #    !is.null(query[[2]]) && inherits(query[[2]], c("numeric", "double", "integer"))) {
    #   lon = query[[1]]
    #   lat = query[[2]]
    # }
      if(inherits(query, "list") && 
         (is.null(query$lat) | is.null(query$lon))) {
        c("Missing `lat` or `lon` values.",
          i = "Point coordinates should be specified using `lat` & `lon` arguments, or supplied as an `sfc_POINT`.") |>
        cli::cli_abort(call = error_call)
      } else {
        query <- query
        
        # extract point coordinate values
        lat <- query$lat
        lon <- query$lon
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
     c("No radius value specified.",
      "*" = "Setting radius to 10 km.") |>
    cli::cli_warn()
    radius <- 10
  } else {
    # Only use one radius value
    if(length(query$radius) > 1) {
      n_radius <- length(query$radius)
      c("More than 1 radius provided.",
             "*" = "Using first radius, ignoring additional {n_radius - 1} value(s).") |>
        cli::cli_warn()
      radius <- query$radius[[1]]
    } else {
      radius <- query$radius
    }
  }
  
  # Check object is accepted class when supplied to lat/lon/radius arguments
  if (!any(inherits(c(lat, lon, radius), c("numeric", "double", "integer")))) {

    wrong_classes <- tibble::tibble(arg = names(query),
                                    class = purrr::map(query, class) |> unlist()) |>
      filter(!class %in% c("numeric", "double", "integer"))
    
    unrecognised_class <- glue::glue_collapse(unique(wrong_classes$class), 
                                              sep = ", ")
    c("Invalid class detected.",
      i = "Point can be specified as numeric `lat` & `lon` coordinates, or supplied as an `sfc_POINT`.",
      x = "`galah_radius()` does not accept type '{unrecognised_class}'.") |>
    cli::cli_abort(call = error_call)
  }
  
  # make sure lat/lon aren't impossible
  if(lon > 180 | lon < -180 | lat > 90 | lat < -90) {
    c("Point location outside of possible range.",
      i = "Are the coordinates valid?") |>
    cli::cli_abort(call = error_call)
  }
  
  # Should this be an error? A message?
  if(radius > 1565) {
    c("Radius is larger than the area of Australia.",
      i = "Try reducing the radius to narrow your query.") |>
    cli::cli_inform()
  }
  
  out_query <- list(lat = lat, 
                    lon = lon, 
                    radius = radius)
  out_query
}