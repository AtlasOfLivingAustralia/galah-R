#' Collect counts
#' @importFrom dplyr bind_rows
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_counts <- function(.data){
  result <- query_API(.data)
  # q: how to add previous columns to the result?
  # this is challenging, might have to have to be handled by `query_API()`
  if(inherits(result, "data.frame")){
    if(nrow(result) > 0){
      # situation with multiple urls
      if(any(colnames(result) == "fieldResult")){
        result <- bind_cols(
          select(result, -fieldName, -fieldResult, -count), # best practice?
          bind_rows(result$fieldResult))
      }
      # resume normal programming
      if(inherits(.data$url, "data.frame")){
        field_name <- url_parse(.data$url$url[[1]])$query$facets
      }else{
        field_name <- url_parse(.data$url)$query$facets
      }
      result_tibble <- result |>
        mutate({{field_name}} := label) |> 
        select(-label, - fq, -i18nCode) # best practice?
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
      result # unclear what circumstances would lead to a zero-row df, if any
    }
  }else{
    tibble(count = result) # no `group_by()`
  }
}