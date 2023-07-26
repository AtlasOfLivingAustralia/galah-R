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
    current_urls <- unlist(.data$url)
    grouping_df <- names(.data$url) |>
      build_facet_labels() |>
      mutate(url = {{current_urls}})
    query_API_multiple(.data, grouping_df)
  # single calls
  }else{
    result <- query_API(.data)
    if(inherits(result, "list")){
      result <- pluck(result, 1, "fieldResult") |>
                bind_rows()
    }
    if(inherits(result, "data.frame")){
      field_name <- attributes(.data)$fields
      result |>
        mutate({{field_name}} := label) |> 
        select({{field_name}}, count) 
    }else{
      tibble(count = result)
    }
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
query_API_multiple <- function(.data, df){
  lapply(split(df, seq_len(nrow(df))),
         function(a){
           data_tr <- .data
           data_tr$url <- a$url
           result <- query_API(data_tr) |>
             pluck(!!!list(1, "fieldResult")) |>
             bind_rows() |>
             build_facet_table()
           bind_cols(select(a, -url), result)
         }) |>
    bind_rows()
}

#' Build facet tables within `collect_counts()`
#' This is about parsing correctly, including converting to factors if needed
#' @param df A tibble
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
build_facet_table <- function(df){
  if(nrow(df) < 1){
    return(NULL)
  }
  x_df <- df |> 
    pull("i18nCode") |>
    string_to_tibble(split_by = "\\.")
  
  if(all(grepl("^[[:digit:]]+$", x_df$value))){
    x_df$value <- as.integer(x_df$value)
    df$label <- factor(
      x_df$value,
      levels = x_df$value,
      labels = df$label)
  }
  
  colnames(df)[1] <- x_df$variable[1]
  df[, c(1, 3)]
}

#' Build facet labels within `collect_counts()`
#' @noRd
#' @keywords Internal
build_facet_labels <- function(string){
  string_to_tibble(string, 
                   split_by = "\\|\\|") |>
    solr_to_tibble()
}

#' function to parse `variable:value` strings to to `tibble(variable = values)`
#' @param x a tibble containing solr queries
#' @importFrom dplyr bind_cols
#' @noRd
#' @keywords Internal
solr_to_tibble <- function(x){
  lapply(x, function(a){
    df <- string_to_tibble(a, split_by = ":")
    df$value <- gsub("\"", "", df$value)
    colnames(df)[2] <- df$variable[1]
    df[, 2]
  }) |>
  bind_cols()
}

#' sub-function to convert strings to tibbles
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
string_to_tibble <- function(string, split_by = ":"){
  x <- strsplit(string, split_by) 
  x_df <- do.call(rbind, x) |> 
    as.data.frame() |>
    tibble()
  colnames(x_df) <- c("variable", "value")
  return(x_df)
}