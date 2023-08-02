#' Collect counts
#' @importFrom dplyr bind_rows
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_counts <- function(.data){
  # if multiple urls given (i.e. group_by() with expand = TRUE), treat separately
  if(length(.data$url) > 1){
    query_API_multiple_counts(.data)
  # single calls
  }else{
    collect_counts_arrange(.data)
  }
}

#' Parse out groups of urls and return queries
#' @param .data an object with relevant headers etc, created by `compute_counts()`
#' @param df A tibble with associated metadata
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
query_API_multiple_counts <- function(.data){
  lapply(split(.data$url, seq_len(nrow(.data$url))),
         function(a, all_data) {
           all_data$url <- a$url
           result <- collect_counts_arrange(all_data)
           if (nrow(result) < 1) {
             NULL
           } else{
             bind_cols(select(a,-url), result)
           }
         }, all_data = .data[names(.data) != "url"]) |>
    bind_rows()
}

#' Internal function to arrange counts properly
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_counts_arrange <- function(.data){
  result <- query_API(.data)
  if(inherits(result, "list")){
    result <- pluck(result, 1, "fieldResult") |>
      bind_rows()
  }
  if(inherits(result, "data.frame") & nrow(result) > 0){
    field_name <- url_parse(.data$url)$query$facets
    result_tibble <- result |>
      mutate({{field_name}} := label) |> 
      select({{field_name}}, count)
    if(.data$arrange$direction == "ascending" & .data$arrange$variable == "count"){
      arrange_var <- .data$arrange$variable
      result_tibble <- result_tibble |>
        arrange(!!as.symbol(arrange_var))
    }
    if(.data$arrange$direction == "descending" & .data$arrange$variable != "count"){
      arrange_var <- .data$arrange$variable
      result_tibble <- result_tibble |>
        arrange(dplyr::desc(!!as.symbol(arrange_var)))
    }
    result_tibble
  }else{
    tibble(count = result)
  } 
}