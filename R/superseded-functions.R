#' Superseded functions
#' 
#' These functions are still valid, but have been superseded by more recent
#' versions. They are designed to be used to supply arguments within 
#' functions from the [atlas_] family. Instead you should consider using
#' piped functions for this same functionality.
#' @name superseded_functions
#' @rdname superseded_functions
#' @aliases galah_filter
#' @aliases galah_identify
#' @details
#' Replacements are as follows:
#' 
#' \itemize{
#'   \item \code{\link[=apply_profile]{apply_profile()}} instead of `galah_apply_profile()`
#'   \item \code{\link[=filter.data_request]{filter()}} instead of `galah_filter()`
#'   \item \code{\link[=geolocate.data_request]{geolocate()}} instead of `galah_geolocate()`
#'   \item \code{\link[=group_by.data_request]{select()}} instead of `galah_group_by()`
#'   \item \code{\link[=identify.data_request]{select()}} instead of `galah_identify()`
#'   \item \code{\link[=select.data_request]{select()}} instead of `galah_select()`
#' }
NULL