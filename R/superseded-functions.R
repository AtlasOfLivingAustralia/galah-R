#' Superseded functions
#' 
#' These functions are still valid, but have been superseded by more recent
#' versions. They are designed to be used to supply arguments within 
#' functions from the [atlas_] family. Instead you should consider using
#' piped functions for this same functionality.
#' @name superseded_functions
#' @rdname superseded_functions
#' @param ... Arguments passed to the function in question, usually (but 
#' not always) using non-standard evaluation.
#' @param type `string`: one of `"polygon"`, `"bbox"` or `"radius"`. Defaults to
#' `"polygon"`. If `type = "polygon"`, a multipolygon will be built via 
#' [geolocate_polygon()]. If `type = "bbox"`, a multipolygon will be built via 
#' [geolocate_bbox()]. The multipolygon is used to narrow a query to the ALA.
#' @param group `string`: (optional) name of one or more column groups to
#' include. Valid options are `"basic"`, `"event"` `"taxonomy"`, `"media"` and
#' `"assertions"`.
#' @aliases galah_apply_profile
#' @aliases galah_filter
#' @aliases galah_geolocate
#' @aliases galah_polygon
#' @aliases galah_bbox
#' @aliases galah_radius
#' @aliases galah_group_by
#' @aliases galah_identify
#' @aliases galah_select
#' @details
#' Replacements are as follows:
#' 
#' \itemize{
#'   \item \code{\link[=apply_profile]{apply_profile()}} instead of `galah_apply_profile()`
#'   \item \code{\link[=filter.data_request]{filter()}} instead of `galah_filter()`
#'   \item [geolocate()] instead of `galah_geolocate()`
#'   \item [geolocate_polygon()] instead of `galah_polygon()`
#'   \item [geolocate_bbox()] instead of `galah_bbox()`
#'   \item [geolocate_radius()] instead of `galah_radius()`
#'   \item \code{\link[=group_by.data_request]{group_by()}} instead of `galah_group_by()`
#'   \item \code{\link[=identify.data_request]{identify()}} instead of `galah_identify()`
#'   \item \code{\link[=select.data_request]{select()}} instead of `galah_select()`
#' }
NULL

#' @rdname superseded_functions
#' @order 1
#' @export
galah_apply_profile <- function(...){
  lifecycle::deprecate_warn("2.3.0", "galah_apply_profile()", "apply_profile()")
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           result <- parse_quosures_basic(dots[-1]) |>
             parse_profile()
           update_request_object(dots[[1]],
                                 apply_profile = result)
         },
         {
           parse_quosures_basic(dots) |>
             parse_profile()
         })
}

#' @rdname superseded_functions
#' @order 2
#' @export
galah_filter <- function(...){
  lifecycle::deprecate_warn("2.3.0", "galah_filter()", "filter()")
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  check_named_input(dots)
  switch(class(dots[[1]])[1],
         "data_request" = {
           if(dots[[1]]$atlas == "Global"){
             filters <- parse_quosures_data_gbif(dots[-1])  # `handle_quosures_GBIF.R`
           }else{
             filters <- parse_quosures_data(dots[-1]) # `handle_quosures.R`
           }
           update_request_object(dots[[1]],
                                 filter = filters)
         },
         "metadata_request" = {
           parse_quosures_metadata(dots[[1]], dots[-1])
         },
         "files_request" = {
           input <- dots[[1]]
           parsed_dots <- parse_quosures_files(dots[-1])
           input$filter <- parsed_dots$data
           input$type <- parsed_dots$variable
           input
         },
         # NOTE: below here is triggered if user calls `galah_filter()` without
         # `galah_call()`/`request_data()` first. In this case we only have
         # global settings to go on.
         if(potions::pour("atlas", "region", .pkg = "galah") == "Global"){
           parse_quosures_data_gbif(dots)
         }else{
           parse_quosures_data(dots)  
         }
  )
}

#' @rdname superseded_functions
#' @order 3
#' @export
galah_geolocate <- function(...) {
  lifecycle::deprecate_warn("2.3.0", "galah_geolocate()", "geolocate()")
  geolocate(...)
}

#' @rdname superseded_functions
#' @order 4
#' @export
galah_polygon <- function(...) {
  lifecycle::deprecate_warn("2.3.0", "galah_polygon()", "geolocate_polygon()")
  geolocate_polygon(...)
}

#' @rdname superseded_functions
#' @order 5
#' @export
galah_bbox <- function(...) {
  lifecycle::deprecate_warn("2.3.0", "galah_bbox()", "geolocate_bbox()")
  geolocate_bbox(...)
}

#' @rdname superseded_functions
#' @order 6
#' @export
galah_radius <- function(...) {
  lifecycle::deprecate_warn("2.3.0", "galah_radius()", "geolocate_radius()")
  geolocate_radius(...)
}

#' @rdname superseded_functions
#' @order 7
#' @export
galah_group_by <- function(...){
  lifecycle::deprecate_warn("2.3.0", "galah_group_by()", "group_by()")
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           df <- parse_quosures_basic(dots[-1]) |>
             parse_group_by()
           update_request_object(dots[[1]],
                                 group_by = df)
         },
         {
           parse_quosures_basic(dots) |>
             parse_group_by()
         })
}

#' @rdname superseded_functions
#' @order 8
#' @export
galah_identify <- function(...) {
  lifecycle::deprecate_warn("2.3.0", "galah_identify()", "identify()")
  dots_initial <- list(...)
  if (length(dots_initial) < 1) {
    cli::cli_warn("No query passed to `identify()`.")
    tibble::tibble("search_term" = character())
  }else{
    if(inherits(dots_initial[[1]], "data_request")){
      do.call(identify.data_request, dots_initial)
    }else{
      search_terms <- identify(galah_call(), ...)$identify
      return(search_terms)
    }
  }
}

#' @rdname superseded_functions
#' @order 9
#' @export
galah_select <- function(..., group = NULL){
  lifecycle::deprecate_warn("2.3.0", "galah_select()", "select()")
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object() |>
    as.list()
  if(length(dots) < 1){
    list(quosure = c(), summary = "") |>
      add_group(group)
  }
  else if(inherits(dots[[1]], "data_request")){
    list(quosure = dots[-1],
          summary = generate_summary(dots[-1])) |>
      add_group(group) |>
    update_request_object(dots[[1]],
                          select = _) 
  }else{
    list(quosure = dots,
          summary = generate_summary(dots)) |>
      add_group(group)
  } 
}