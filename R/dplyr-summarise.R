#' Summarise each group down to one row
#' 
#' `r lifecycle::badge("experimental")`
#' `summarise()` creates a new data frame. It returns one row for each combination 
#' of grouping variables; if there are no grouping variables, the output will 
#' have a single row summarising all observations in the input. It will contain 
#' one column for each grouping variable and one column for each of the summary 
#' statistics that you have specified.
#' 
#' Like all `dplyr` extensions in `galah`, this function amends a `data_request`,
#' `metadata_request` or `files_request`, and is evaluated lazily.
#' 
#' `summarise()` and `summarize()` are synonyms.
#' @name summarise.data_request
#' @order 1
#' @param ... Name-value pairs of summary functions. The name will be the 
#' name of the variable in the result. The value can be a single function
#' such as `min(x)`, `n()`, or `sum()`
#' @export
summarise.data_request <- function(.data, ...){
  cli::cli_abort("`summarise()` is not yet supported in `galah`. Please try again later.")
}


#' @name summarise.data_request
#' @order 2
#' @export
summarize.data_request <- summarise.data_request