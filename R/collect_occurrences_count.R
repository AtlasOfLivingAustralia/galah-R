#' Collect counts
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
collect_counts <- function(.data){
  result <- query_API(.data)
  if(length(result$facetResults) < 1 & !is.null(result$totalRecords)){ # first handle single values
    tibble(count = result$totalRecords)
  }else{  # then when group_by() is specified
    clean_group_by(result, .data) |>
      bind_rows() |>
      clean_labels() |>
      arrange_counts(direction = .data$arrange$direction,
                     variable = .data$arrange$variable)
  }
}

#' Internal function to clean objects returned by group_by()
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom purrr pluck
#' @returns A list
#' @noRd
#' @keywords Internal
clean_group_by <- function(result, .data){
  access_list <- list(1, "fieldResult")
  if(inherits(.data$url, "data.frame")){
    added_cols <- select(.data$url, -url)  
    lapply(seq_along(result), function(a){
      # get list-cols and convert to df
      result_df <- pluck(result[[a]], !!!access_list) |> 
        bind_rows()
      # add supplied cols
      added_cols |>
        slice(a) |>
        bind_cols(result_df)      
    })
  }else{
    pluck(result, !!!access_list)
  }
}

#' Internal function to clean up columns when group_by() is specified
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
clean_labels <- function(df){
  if(all(c("label", "i18nCode") %in% colnames(df))){
    dot_placement <- regexpr("\\.", df$i18nCode[1]) |>
      as.integer()
    field_name <- substr(df$i18nCode[1], 
                         start = 1, 
                         stop = dot_placement[1] - 1)
    df |>
      rename({{field_name}} := label) |>
      select(-fq, -i18nCode)
  }else{
    df
  }
}

#' Internal function to arrange count df
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @noRd
#' @keywords Internal
arrange_counts <- function(df, 
                           direction = "descending",
                           variable = "count"){
  var_symbol <- as.symbol(variable)
  if(direction == "ascending" & variable == "count"){
    arrange(df, !!var_symbol)
  } else if(direction == "descending" & variable != "count"){
    arrange(df, dplyr::desc(!!var_symbol))
  } else {
    df
  }
}