#' List valid options for a categorical field
#'
#' When building a set of filters with \code{\link{select_filters}}, a user can
#' use this function to check that the values provided are valid options.
#' @param field \code{string}: field to return the categories for. Use
#' \code{\link{search_fields}} to view valid fields.
#' @param limit \code{numeric}: maximum number of categories to return. 20 by default.
#' @return A \code{data.frame} containing columns \code{field} (user-supplied)
#' and \code{category} (i.e. field values).
#' @seealso See \code{\link{search_fields}} for ways to use information returned
#' by this function.
#' @examples \dontrun{
#' find_field_values("basis_of_record")
#' find_field_values("state")
#' }
#' @export find_field_values

find_field_values <- function(field, limit = 20) {
  if (missing(field)) {
    stop("`find_field_values` requires a field to search for")
  }
  field <- dwc_to_ala(field)
  if (!(field %in% all_fields()$name)) {
    stop("\"", field,
         "\" is not a valid field. See valid fields with `search_fields()`.")
  }
  assert_that(is.numeric(limit))
  url <- getOption("galah_server_config")$base_url_biocache
  resp <- ala_GET(url, "ws/occurrence/facets",
                    params = list(facets = field, flimit = limit))

  if (resp$count > limit) {
    warning("This field has ", resp$count, " possible values. Only the first ",
    limit, " will be returned. Change `limit` to return more values.")
  }
  category <- vapply(resp$fieldResult[[1]]$fq, function(n) {
    extract_category_value(n)
  }, USE.NAMES = FALSE, FUN.VALUE = character(1))
  cbind(field = field, as.data.frame(category))
}

# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}
