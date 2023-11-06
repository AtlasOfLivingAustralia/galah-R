#' @rdname galah_geolocate
#' @export
galah_polygon <- function(...){
  # check to see if any of the inputs are a data request
  query <- list(...)
  if(length(query) > 1 & inherits(query[[1]], "data_request")){
    dr <- query[[1]]
    query <- query[-1]
  }else{
    dr <- NULL
  }
  # check that only 1 WKT is supplied at a time
  check_n_inputs(query)
  # parse
  out_query <- parse_polygon(query)
  # if a data request was supplied, return one
  if(!is.null(dr)){
    update_data_request(dr, geolocate = out_query)
  }else{
    out_query
  }   
}

#' @rdname galah_geolocate
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param y A valid Well-Known Text string (wkt), a `POLYGON` or a `MULTIPOLYGON`
#' @export
st_crop.data_request <- function(x, y, ...){
  update_data_request(x, geolocate = parse_polygon(y))
}

#' parser for polygons
#' @importFrom rlang try_fetch
#' @importFrom sf st_as_sfc 
#' @importFrom sf st_is_valid 
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @noRd
#' @keywords Internal
parse_polygon <- function(query){
  # make sure shapefiles are processed correctly
  if (!inherits(query, "sf")) {query <- query[[1]]} else {query <- query}
  
  # check object is accepted class
  if (!inherits(query, c("character", "list", "matrix", "data.frame", "tbl", "sf", "sfc", "XY"))) {
    
    unrecognised_class <- class(query)
    bullets <- c(
      "Invalid object detected.",
      i = "Did you provide a polygon or WKT in the right format?",
      x = glue("`galah_polygon` cannot use object of class '{unrecognised_class}'.")
    )
    abort(bullets, call = caller_env())
  }
  
  # handle shapefiles
  if (inherits(query, "XY")) query <- st_as_sfc(query) 
  
  # make sure spatial object or wkt is valid
  if (!inherits(query, c("sf", "sfc"))) {
    check_wkt_length(query)
    
    # handle errors from converting impossible WKTs
    query <- try_fetch(
      query |> st_as_sfc(), 
      error = function(cnd) {
        bullets <- c(
          "Invalid WKT detected.",
          i = "Check that the spatial feature or WKT in `galah_polygon` is correct."
        )
        abort(bullets, call = caller_env())
      })
    
    # validate that wkt/spatial object is real
    valid <- query |> st_is_valid() }
  else {
    valid <- query |> st_is_valid()
  }
  if(any(is.na(valid))) {
    bullets <- c(
      "Invalid spatial object or WKT detected.",
      i = "Check that the spatial feature or WKT in `galah_polygon` is correct."
    )
    abort(bullets, call = caller_env())
  }
  
  # check number of vertices of WKT
  if(any(n_points(query) > 500)) {
    n_verts <- n_points(query)
    bullets <- c(
      glue("Polygon has too many vertices."),
      i = "`galah_polygon` only accepts simple polygons.",
      i = "See `?sf::st_simplify` for how to simplify geospatial objects.",
      x = "Polygon must have 500 or fewer vertices, not {n_verts}."
    )
    abort(bullets, call = caller_env())
  }
  
  # currently a bug where the ALA doesn't accept some polygons
  # to avoid any issues, any polygons are converted to multipolygons
  if(inherits(query, "sf") || inherits(query, "sfc")) {

    if(length(query$geometry) < 2) {
    out_query <- build_wkt(query)
    } else {
    # multiple polygons
      n_polygons <- length(query$geometry)
      bullets <- c(
        "Too many polygons.",
        i = "`galah_polygon` cannot accept more than 1 polygon at a time.",
        x = glue("{n_polygons} polygons detected in spatial object.")
      )
      abort(bullets, call = caller_env())
      
      ## NOTE: Code below parses multiple polygons. 
      ##       Please do not remove!
      ##       Code works but unsure how to pass to ALA query just yet
    
      # out_query <- query |>
      #   mutate(
      #     wkt_string = map_chr(.x = query$geometry,
      #                          .f = build_wkt),
      #     row_id = dplyr::row_number()) |>
      #   as_tibble() |>
      #   select(row_id, wkt_string)
    }
  } else {
    
    # remove space after "POLYGON" if present
    if(str_detect(query, "POLYGON \\(\\("))
      query <- str_replace(query, "POLYGON \\(\\(", "POLYGON\\(\\(")
    
    if (str_detect(query, "POLYGON") & ! str_detect(query, "MULTIPOLYGON")) {
      # change start of string
      query <- str_replace(query, "POLYGON\\(\\(", "MULTIPOLYGON\\(\\(\\(")
      # add an extra bracket
      query <- paste0(query, ")")
    }
    out_query <- query
  }
  out_query
}

#' Internal function to `galah_polygon`
#' @importFrom sf st_geometry
#' @noRd
#' @keywords Internal
n_points <- function(x) {
  count_vertices(st_geometry(x))
}

#' Internal function to `galah_polygon`
#' @importFrom rlang caller_env
#' @importFrom sf st_is_empty
#' @noRd
#' @keywords Internal
count_vertices <- function(wkt_string, error_call = caller_env()) {
  out <- if (is.list(wkt_string)) 
    sapply(sapply(wkt_string, count_vertices), sum) 
  else {
    if (is.matrix(wkt_string))
        nrow(wkt_string)
    else {
      if (!st_is_empty(wkt_string)) 1 else
        0
      }
  }
  unname(out)
}

#' Internal function to `galah_polygon`
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom rlang is_list
#' @importFrom rlang is_string
#' @noRd
#' @keywords Internal
check_wkt_length <- function(wkt, error_call = caller_env()) {
  if (is_string(wkt) == TRUE |
    is.matrix(wkt) == TRUE  | 
    is_list(wkt) == TRUE | 
    is.data.frame(wkt) == TRUE) {
    # make sure strings aren't too long for API call
    if(!inherits(wkt, "character")){
      abort("Argument `wkt` must be of class 'character'",
            call = error_call)
    }
    n_char_wkt <- nchar(wkt)
    max_char <- 10000
    if (n_char_wkt > max_char) {
      bullets <- c(
        "Invalid WKT detected.",
        x = glue("WKT string can be maximum {max_char} characters. WKT supplied has {n_char_wkt}.")
      )
      abort(bullets, call = error_call)
    }
  } 
}
