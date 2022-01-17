#' Search for valid options of a categorical field
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
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Find valid values you can use to filter or categorise results for the field 
#' "basisOfRecord"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_field_values("basisOfRecord")
#' ```
#' 
#' Find valid values to filter or categorise results for the field 
#' "stateProvince"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_field_values("stateProvince")
#' ```
#' 
#' Use these values to with [galah_filter()] tp filter results of `atlas_` 
#' functions. For example, we can return the number of records only from 
#' Tasmania
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() |>
#'   galah_filter(stateProvince == "Tasmania") |>
#'   atlas_counts()
#' ```
#' 
#' @export search_field_values

search_field_values <- function(field, limit = 20) {
  if (missing(field)) {
    bullets <- c(
      "Nothing field detected.",
      i = "Did you forget to add a field to search for?"
    )
    abort(bullets, call = caller_env())
  }

  if (!(field %in% all_fields()$name)) {
    bullets <- c(
      "Invalid field detected.",
      i = "Search for the valid name of a desired field with `search_fields()`.",
      x = glue("\"{field}\" is not a valid field.")
    )
    abort(bullets, call = caller_env())
  }
  assert_that(is.numeric(limit))
  url <- server_config("records_base_url")
  resp <- atlas_GET(url, "occurrence/facets",
                    params = list(facets = field, flimit = limit))
  if(is.null(resp)){
    bullets <- c(
      "Calling the API failed for `atlas_taxonomy`.",
      i = "This might mean that the ALA system is down. Double check that your query is correct."
      )
    inform(bullets)
    return(tibble())
  }else{
    if (resp$count > limit & galah_config()$verbose) {
      values <- resp$count
      bullets <- c(
        glue("This field has {values} possible values. Only the first {limit} will be returned."),
        i = "Change `limit` to return more values."
      )
      warn(bullets)
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
