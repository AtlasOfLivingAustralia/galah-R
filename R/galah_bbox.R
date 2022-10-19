#' Narrow a query to within a bounding box
#'
#' Restrict results to within a bounding box (a box constructed from min/max 
#' latitude & longitude coordinates). 
#' Bounding boxes can be extracted from a supplied `sf`/`sfc` object or 
#' a shapefile. A bounding box can also be supplied as a `bbox` object 
#' (via `sf::st_bbox`) or a `tibble`/`data.frame`.
#'
#' @param ... an `sf` object or a shapefile (.shp), or bounding box coordinates 
#' supplied as a `bbox`, a `tibble`/`data.frame`
#' @details an `sf` object or a shapefile polygon will be simplified to its 
#' bbox coordinates. A bounding box can be supplied as a `bbox` object or as 
#' a `tibble`/`data.frame`. Bounding boxes supplied as a `tibble`/`data.frame` 
#' must have "xmin", "xmax", "ymin" and "ymax" columns with valid `numeric` 
#' values.
#' @return length-1 object of class `character` and `galah_geolocate`,
#' containing a multipolygon WKT string representing the bounding box of the 
#' area provided.
#' @seealso [galah_polygon()] & [galah_geolocate()] for other ways to narrow
#' queries by location. See [search_taxa()], [galah_filter()] and
#' [galah_select()] for other ways to restrict the information
#' returned by [atlas_occurrences()] and related functions.
#'
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#'
#' Search for records using a bounding box of coordinates
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' b_box <- sf::st_bbox(c(xmin = 143, xmax = 148, ymin = -29, ymax = -28), 
#'                      crs = st_crs("WGS84"))
#'
#' galah_call() |>
#'   galah_identify("reptilia") |>
#'   galah_bbox(b_box) |>
#'   atlas_counts()
#' ```
#'
#' Search for records using a bounding box in a `tibble` or `data.frame`
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' b_box <- tibble(xmin = 148, ymin = -29, xmax = 143, ymax = -21)
#'
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_bbox(b_box) |>
#'   atlas_counts()
#' ```
#'
#' Search for records within the bounding box of an `sf` object
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(email = "your-email@email.com")
#'
#' location <- 
#' "POLYGON((143.32 -18.78,145.30 -20.52,141.52 -21.50,143.32 -18.78))" |>
#'  sf::st_as_sfc()
#'  
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_bbox(location) |>
#'   atlas_occurrences()
#' ```
#' 
#'  
#' Search for records within the bounding box of a shapefile
#'
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(email = "your-email@email.com")
#'
#' location <- sf::st_read(path/to/shapefile.shp)
#' galah_call() |>
#'   galah_identify("vulpes") |>
#'   galah_bbox(location) |>
#'   atlas_occurrences()
#' ```
#'
#' @importFrom sf st_cast st_as_text st_as_sfc st_is_empty st_is_simple
#' @importFrom sf st_is_valid st_bbox st_geometry_type
#' @importFrom rlang try_fetch
#' 
#' @keywords internal
#' 
#' @export
galah_bbox <- function(...) {

  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")
  checked_dots <- detect_data_request(dots)
  if (!inherits(checked_dots, "quosures")) {
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  } else {
    is_data_request <- FALSE
  }

  # accept one input only
  check_n_inputs(dots)

  # convert dots to query
  query <- parse_basic_quosures(dots[1])

  # process shapefiles correctly
  if (!inherits(query, "sf")) {
    if (!inherits(query, c("tbl", "data.frame", "matrix"))) {
      query <- query[[1]]
    }
  } else {
    query <- query
  }

  # if(exists("query") == FALSE) {abort(glue("Object {{query}} not found."))} # need something for if blank

  # check that object is accepted class
  if (!inherits(query, c("bbox", "list", "matrix", "data.frame", "tbl", "sf", "sfc", "XY"))) {
    unrecognised_class <- class(query)
    bullets <- c(
      "`galah_bbox` input must be an sf object, data.frame or tibble.",
      x = glue("Can't use object of class '{unrecognised_class}'.")
    )
    if (inherits(query, "character")) {
      suggest <- c(
        i = "Did you mean to use `galah_polygon`?"
      )
      abort(c(bullets, suggest), call = caller_env())
    } else {
      abort(bullets, call = caller_env())
    }
  }

  # handle shapefiles
  if (inherits(query, "XY")) query <- sf::st_as_sfc(query)

  # validate spatial objects & coordinates
  if (!inherits(query, c("sf", "sfc"))) {
    if(inherits(query, c("tbl", "data.frame"))) {
      check_col_names(query)
      query <- check_n_rows(query)
      query <- st_bbox(c(xmin = query$xmin,
                         xmax = query$xmax,
                         ymin = query$ymin,
                         ymax = query$ymax),
                       crs = st_crs("WGS84"))
    }
    log <- NULL # see `log` to read any warnings that may have been silenced
    valid <- rlang::try_fetch( # prevent warnings
      query |>
        st_as_sfc() |>
        st_is_valid(), warning = function(cnd) {
          log <<- cnd
          ""
        })
  } 
  else {
    valid <- query |> st_is_valid()
  }
  
  if (valid != TRUE) {
    bullets <- c(
      "Invalid spatial object or WKT detected.",
      i = "Check that the spatial feature or bounding box in `galah_bbox` is correct."
    )
    abort(bullets, call = caller_env())
  } else {
    if (inherits(query, c("tbl", "data.frame", "bbox")) && !inherits(query, c("sf", "sfc"))) {
      bbox_coords <- round(query, 5)
      query <- query |> st_as_sfc(crs = st_crs("WGS84"))
    } else {
      if (inherits(query, c("sf", "sfc"))) {
        query <- query |> st_bbox(crs = st_crs("WGS84"))
        bbox_coords <- round(query, 5)
        query <- query |> st_as_sfc(crs = st_crs("WGS84")) # FIXME: should we define the projection?
      }
    }
  }


  # currently a bug where the ALA doesn't accept some polygons
  # to avoid any issues, any polygons are converted to multipolygons
  if (inherits(query, "sf") || inherits(query, "sfc")) {
    inform(glue::glue("
             Data returned for bounding box:
             xmin = {bbox_coords$xmin} xmax = {bbox_coords$xmax} \\
             ymin = {bbox_coords$ymin} ymax = {bbox_coords$ymax}"))
    out_query <- build_wkt(query)
  }

  attr(out_query, "bbox") <- bbox_coords
  attr(out_query, "call") <- "galah_geolocate"

  # if a data request was supplied, return one
  if (is_data_request) {
    update_galah_call(data_request, geolocate = out_query)
  } else {
    out_query
  }
}


# build a valid wkt string from a spatial polygon
build_wkt <- function(polygon, error_call = caller_env()) {
  if (st_geometry_type(polygon) == "POLYGON") {
    polygon <- st_cast(polygon, "MULTIPOLYGON")
  }
  if (!st_is_simple(polygon)) {
    bullets <- c(
      "The area provided to `galah_bbox` is too complex. ",
      i = "See `?sf::st_simplify` for how to simplify geospatial objects."
    )
    abort(bullets, call = caller_env())
  }
  wkt <- st_as_text(st_geometry(polygon))
  wkt
}


check_n_rows <- function(tibble) {
  if (is.null(nrow(tibble))) {
    tibble <- tibble
  } 
  else {if (nrow(tibble) > 1) {
    ignored_rows <- paste(2:(nrow(tibble)))
    bullets <- c(
      "More than 1 set of coordinates supplied to `galah_bbox`.",
      "*" = glue("Using first row, ignoring row(s) {ignored_rows}.")
    )
    warn(bullets)
    tibble <- tibble[1, ]
  } else {
    tibble <- tibble
  }}
  return(tibble)
}

check_col_names <- function(tibble, error_call = caller_env()) {
  valid_col_names <- c("xmin", "xmax", "ymin", "ymax")
  if (!identical(sort(names(tibble)), sort(valid_col_names))) {
    col_names <- names(tibble[!names(tibble) %in% valid_col_names])
    col_names <- glue::glue_collapse(col_names,
      sep = ", "
    )
    bullets <- c(
      "Incorrect column names supplied to `galah_bbox`.",
      i = "Column names must be: 'xmin', 'xmax', 'ymin', 'ymax'.",
      x = glue("Unrecognised column name(s): '{col_names}'.")
    )
    abort(bullets, call = error_call)
  }
}

# nesting_depth <- function(object, object_depth = 0) {
#   if (!is.list(object)) {
#     return(object_depth)
#   } else {
#     return(
#       max(
#         unlist(
#           lapply(object, nesting_depth, object_depth = object_depth + 1)
#         )
#       )
#     )
#   }
# }
