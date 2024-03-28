#' `collect()` for `type = "data/occurrences-count"`
#' @noRd
#' @keywords Internal
collect_occurrences_count <- function(.query){
  if(is_gbif()){
    collect_occurrences_count_gbif(.query)
  }else{
    collect_occurrences_count_la(.query)
  }
}

#' `collect()` for `type = "data/occurrences-count"` for gbif
#' @importFrom dplyr bind_rows
#' @importFrom httr2 url_parse
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_occurrences_count_gbif <- function(.query){
  result <- query_API(.query)
  if(length(result$facets) < 1 & !is.null(result$count)){ # first handle single values
    tibble(count = result$count)
  }else{
    # note: this only works for length(facets) == 1
    result_df <- result |>
      pluck(!!!list("facets", 1, "counts")) |>
      bind_rows()
    names(result_df)[1] <- .query$url |> 
      url_parse() |> 
      pluck("query", "facet")
    # names(result_df)[1] <- result |>
    #   pluck(!!!list("facets", 1, "field")) |>
    #   tolower()
    result_df
  }
}
  
#'  `collect()` for `type = "data/occurrences-count"` for living atlases
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @noRd
#' @keywords Internal
collect_occurrences_count_la <- function(.query){
  result <- query_API(.query)
  if(length(result$facetResults) < 1 & !is.null(result$totalRecords)){ # first handle single values
    tibble(count = result$totalRecords)
  }else{  # then when group_by() is specified
    clean_group_by(result, .query) |>
      bind_rows() |>
      clean_labels() |>
      arrange_counts(direction = .query$arrange$direction,
                     variable = .query$arrange$variable)
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
clean_group_by <- function(result, .query){
  access_list <- list(1, "fieldResult")
  if(inherits(.query$url, "data.frame")){
    added_cols <- select(.query$url, -url)  
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
#' @importFrom dplyr all_of
#' @importFrom dplyr any_of
#' @importFrom dplyr last_col
#' @importFrom dplyr relocate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @noRd
#' @keywords Internal
clean_labels <- function(df){
  if(any(colnames(df) == "i18nCode")){
    values <- df$i18nCode |>
      str_extract("\\.([:graph:]|\\s)+$") |>
      str_replace("^\\.", "")
    variable <- df$i18nCode[1] |>
      str_extract("^[:graph:]+\\.") |>
      str_replace("\\.$", "")
    df[[variable]] <- values
    df |>
      select(-any_of(c("label", "i18nCode", "fq"))) |>
      relocate("count", .after = last_col())
  }else{ 
    # Some atlases (e.g. Estonia) only have "label" column
    if(any(colnames(df) == "label")){
      field_name <- str_extract(df$fq[1], "[^:]+") |> 
        as.character()
      col_lookup <- c("label")
      names(col_lookup) <- field_name
      df |>
        rename(all_of(col_lookup)) |>
        select(-"fq")      
    }else{
    # some are completely empty
      df
    }
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
