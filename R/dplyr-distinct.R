#' Keep distinct/unique rows
#' 
#' Keep only unique/distinct rows from a data frame. This is similar to 
#' [unique.data.frame()] but considerably faster. It is evaluated lazily.
#' @param .data A data frame, data frame extension (e.g. a tibble), or a 
#' lazy data frame (e.g. from dbplyr or dtplyr). See Methods, below, 
#' for more details.
#' @param ... Optional variables to use when determining uniqueness. If there 
#' are multiple rows for a given combination of inputs, only the first row 
#' will be preserved. If omitted, will use all variables in the data frame.
#' @param .keep_all If `TRUE`, keep all variables in .data. If a combination 
#' of `...` is not distinct, this keeps the first row of values.
#' @export
distinct.data_request <- function(.data, 
                                  ...,
                                  .keep_all = FALSE){
  # NOTE: internally this is based on `group_by.data_request`
  # BUT there are cases where we need to distinguish between `group_by()` and `distinct()`,
  # hence the separate slots
  parsed_dots <- rlang::enquos(...,
                               .ignore_empty = "all") |>
    parse_quosures_basic() |>
    parse_distinct(keep_all = .keep_all)

  update_request_object(.data,
                        distinct = parsed_dots)
}

#' Internal parsing of `distinct` args
#' @noRd
#' @keywords Internal
parse_distinct <- function(dot_names, 
                           keep_all = FALSE,
                           error_call = rlang::caller_env()){
  if(length(dot_names) > 0){
    if(length(dot_names) > 1){
      c(
        "Too many fields supplied.",
        i = "`distinct.data_request` only accepts one field.") |>
        cli::cli_abort(call = error_call)
    }
    if(length(dot_names) > 0){
      names(dot_names) <- NULL # needed to avoid empty strings added as names
      tibble::tibble(name = dot_names,
                     keep_all = keep_all) 
    }else{
      tibble::tibble(name = NA,
                     keep_all = keep_all)
    }
  }else{
    tibble::tibble(name = NA,
                   keep_all = keep_all)
  }
}