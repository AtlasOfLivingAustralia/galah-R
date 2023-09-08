#' Internal function to call `compute` for `request_values(type = "fields")`
#' @noRd
#' @keywords Internal
compute_field_values <- function(.data){
  check_fields(.data)
  class(.data) <- "values_response"
  .data
}