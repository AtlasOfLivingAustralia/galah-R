#' Internal function to run `compute()` for `request_values(type = "fields")`
#' @importFrom dplyr bind_rows
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_field_values <- function(.data){
  if(is_gbif()){
    .data |>
      query_API()
    # tibble(result$facets$counts[[1]]) # not updated
  }else{
    .data |>
      query_API() |>
      pluck(!!!list(1, "fieldResult")) |>
      bind_rows() |>
      select(label) |>
      rename(field = label) 
  }
}