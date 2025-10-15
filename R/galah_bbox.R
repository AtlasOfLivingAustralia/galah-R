#' @rdname geolocate
#' @order 4
#' @export
galah_bbox <- function(...) {

  # check to see if any of the inputs are a data request
  query <- list(...)
  if(length(query) > 1 & inherits(query[[1]], "data_request")){
    dr <- query[[1]]
    query <- query[-1]
  }else{
    dr <- NULL
  }
  
  # accept one input only
  check_n_inputs(query)

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
      x = "Can't use object of class '{unrecognised_class}'.")
    if (inherits(query, "character")) {
      suggest <- c(
        i = "Did you mean to use `galah_polygon`?"
      )
      cli::cli_abort(c(bullets, suggest),
                     call = rlang::caller_env())
    } else {
      cli::cli_abort(bullets,
                     call = rlang::caller_env())
    }
  }

  # handle shapefiles
  if (inherits(query, "XY")) query <- sf::st_as_sfc(query)

  # validate spatial objects & coordinates
  if (!inherits(query, c("sf", "sfc"))) {
    if(inherits(query, c("tbl", "data.frame"))) {
      check_col_names(query)
      query <- check_n_rows(query)
      query <- sf::st_bbox(c(xmin = query$xmin,
                         xmax = query$xmax,
                         ymin = query$ymin,
                         ymax = query$ymax),
                       crs = sf::st_crs("WGS84"))
    }
    log <- NULL # see `log` to read any warnings that may have been silenced
    valid <- rlang::try_fetch( # prevent warnings
      query |>
        sf::st_as_sfc() |>
        sf::st_is_valid(), warning = function(cnd) {
          log <<- cnd
          ""
        })
  } 
  else {
    valid <- query |> 
      sf::st_is_valid()
  }
  
  if (valid != TRUE) {
    bullets <- c(
      "Invalid spatial object or WKT detected.",
      i = "Check that the spatial feature or bounding box in `galah_bbox` is correct."
    )
    cli::cli_abort(bullets, call = rlang::caller_env())
  } else {
    if (inherits(query, c("tbl", "data.frame", "bbox")) && 
        !inherits(query, c("sf", "sfc"))) {
      bbox_coords <- round(query, 5)
      query <- query |> 
        sf::st_as_sfc(crs = sf::st_crs("WGS84"))
    } else {
      if (inherits(query, c("sf", "sfc"))) {
        query <- query |> 
          sf::st_bbox(crs = st_crs("WGS84"))
        bbox_coords <- round(query, 5)
        query <- query |> 
          sf::st_as_sfc(crs = st_crs("WGS84")) # FIXME: should we define the projection?
      }
    }
  }

  # currently a bug where the ALA doesn't accept some polygons
  # to avoid any issues, any polygons are converted to multipolygons
  if (inherits(query, "sf") || inherits(query, "sfc")) {
    cli::cli_inform("
             Data returned for bounding box:
             xmin = {bbox_coords$xmin} xmax = {bbox_coords$xmax} \\
             ymin = {bbox_coords$ymin} ymax = {bbox_coords$ymax}")
    out_query <- build_wkt(query)
  }

  attr(out_query, "bbox") <- bbox_coords

  # if a data request was supplied, return one
  if(is.null(dr)){
    out_query
  }else{
    update_request_object(dr,
                          geolocate = out_query)
  }
}

#' Internal function to `galah_bbox`
#' @noRd
#' @keywords Internal
check_n_rows <- function(tibble) {
  if (is.null(nrow(tibble))) {
    tibble <- tibble
  }else{
    if (nrow(tibble) > 1) {
      ignored_rows <- paste(2:(nrow(tibble)))
      bullets <- c(
        "More than 1 set of coordinates supplied to `galah_bbox`.",
        "*" = "Using first row, ignoring row(s) {ignored_rows}.")
      cli::cli_warn(bullets)
      tibble <- tibble[1, ]
    }else{
      tibble <- tibble
    }
  }
  return(tibble)
}

#' Internal function to `galah_bbox`
#' @noRd
#' @keywords Internal
check_col_names <- function(tibble,
                            error_call = rlang::caller_env()) {
  valid_col_names <- c("xmin", "xmax", "ymin", "ymax")
  if (!identical(sort(names(tibble)), sort(valid_col_names))) {
    col_names <- names(tibble[!names(tibble) %in% valid_col_names])
    col_names <- glue::glue_collapse(col_names,
      sep = ", "
    )
    bullets <- c(
      "Incorrect column names supplied to `galah_bbox`.",
      i = "Column names must be: 'xmin', 'xmax', 'ymin', 'ymax'.",
      x = "Unrecognised column name(s): '{col_names}'.")
    cli::cli_abort(bullets, call = error_call)
  }
}
