#' List valid options for a categorical field
#'
#' When building a set of filters with [galah_filter()], a user can
#' use this function to check that the values provided are valid options.
#' @param field `string`: field to return the categories for. Use
#' [search_fields()] to view valid fields.
#' @param limit `numeric`: maximum number of categories to return. 20 by default.
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) 
#' containing columns `field` (user-supplied) and `category` (i.e. field values).
#' @seealso See [search_fields()] for ways to use information returned
#' by this function.
#' @examples
#' find_field_values("basisOfRecord")
#' find_field_values("stateProvince")
#' 
#' @export find_field_values

find_field_values <- function(field, limit = 20) {
  if (missing(field)) {
    stop("`find_field_values` requires a field to search for")
  }

  if (!(field %in% all_fields()$name)) {
    stop("\"", field,
         "\" is not a valid field. See valid fields with `search_fields()`.")
  }
  assert_that(is.numeric(limit))
  url <- server_config("records_base_url")
  resp <- atlas_GET(url, "occurrence/facets",
                    params = list(facets = field, flimit = limit))
  if(is.null(resp)){
    inform("Calling the API failed for `find_field_values`")
    return(tibble())
  }else{
    if (resp$count > limit & galah_config()$verbose) {
      warning("This field has ", resp$count, " possible values. Only the first ",
      limit, " will be returned. Change `limit` to return more values.")
    }
    category <- vapply(resp$fieldResult[[1]]$fq, function(n) {
      extract_category_value(n)
    }, USE.NAMES = FALSE, FUN.VALUE = character(1))
    cbind(field = field, as.data.frame(category)) |> as_tibble()
  }
}

# function to extract value which for some reason isn't returned
extract_category_value <- function(name) {
  str_split(name, '"')[[1]][2]
}
