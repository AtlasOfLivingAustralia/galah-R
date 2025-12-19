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
#' @noRd
#' @keywords Internal
collect_occurrences_count_gbif <- function(.query,
                                           error_call = rlang::caller_env()){
  # get response from GBIF
  result <- query_API(.query)
  
  # first handle case when there are multiple queries in a tibble
  if(inherits(result, "data.frame")){
    split(result, seq_len(nrow(result))) |>
      purrr::map(.f = \(a){
        tibble::tibble(
          dplyr::select(a, -"predicate", -"result"),
          collect_occurrences_count_gbif_single(a$result[[1]], 
                                                error_on_null = FALSE))
      }) |>
      dplyr::bind_rows()
  # then handle 'simple' queries
  }else{
    collect_occurrences_count_gbif_single(result,
                                          error_on_null = TRUE)
  }
}

#' collect a single count query
#' @noRd
#' @keywords Internal
collect_occurrences_count_gbif_single <- function(result,
                                                  error_on_null = TRUE,
                                                  error_call = rlang::caller_env()){
  # handle obvious errors
  if(is.null(result$count) & error_on_null){
    cli::cli_abort("API returned a NULL result", 
                   call = error_call)
  }
  
  # parse results
  if(length(result$facets) < 1){ # first handle single values
    tibble::tibble(count = result$count)
  }else{
    purrr::map(
      purrr::pluck(result, "facets"),
      \(a){
        df <- a |>
          purrr::pluck("counts") |>
          dplyr::bind_rows()
        names(df)[1] <- purrr::pluck(a, "field") |>
          snake_to_camel_case()
        df
      }) |>
      dplyr::bind_rows() |>
      dplyr::relocate("count", 
                      .after = dplyr::last_col())
  }
}
  
#'  `collect()` for `type = "data/occurrences-count"` for living atlases
#' @noRd
#' @keywords Internal
collect_occurrences_count_la <- function(.query){
  result <- query_API(.query)
  if(length(result$facetResults) < 1 & !is.null(result$totalRecords)){ # first handle single values
    tibble::tibble(count = result$totalRecords)
  }else{  # then when group_by() is specified
    clean_group_by(result, .query) |>
      dplyr::bind_rows() |>
      clean_labels() |>
      arrange_counts(direction = .query$arrange$direction,
                     variable = .query$arrange$variable)
  }
}

#' Internal function to clean objects returned by group_by()
#' @returns A list
#' @noRd
#' @keywords Internal
clean_group_by <- function(result, .query){
  access_list <- list(1, "fieldResult")
  if(inherits(.query$url, "data.frame")){
    added_cols <- dplyr::select(.query$url, -url)  
    purrr::map(seq_along(result), \(a){
      # get list-cols and convert to df
      result_df <- result[[a]] |>
        purrr::pluck(!!!access_list) |> 
        dplyr::bind_rows()
      # add supplied cols
      added_cols |>
        dplyr::slice(a) |>
        dplyr::bind_cols(result_df)      
    })
  }else{
    purrr::pluck(result, !!!access_list)
  }
}

#' Internal function to clean up columns when group_by() is specified
#' @noRd
#' @keywords Internal
clean_labels <- function(df){
  if(any(colnames(df) == "i18nCode")){
    values <- df$i18nCode |>
      stringr::str_extract("\\.([:graph:]|\\s)+$") |>
      stringr::str_replace("^\\.", "")
    variable <- df$i18nCode[1] |>
      stringr::str_extract("^[:alnum:]+\\.") |>
      stringr::str_replace("\\.$", "")
    df[[variable]] <- values
    df |>
      dplyr::select(-dplyr::any_of(c("label", "i18nCode", "fq"))) |>
      dplyr::relocate("count", 
                      .after = dplyr::last_col())
  }else{ 
    # Some atlases (e.g. Estonia) only have "label" column
    if(any(colnames(df) == "label")){
      field_name <- stringr::str_extract(df$fq[1], "[^:]+") |> 
        as.character()
      col_lookup <- c("label")
      names(col_lookup) <- field_name
      df |>
        dplyr::rename(dplyr::all_of(col_lookup)) |>
        dplyr::select(-"fq")      
    }else{
    # some are completely empty
      df
    }
  }
}

#' Internal function to arrange count df
#' @noRd
#' @keywords Internal
arrange_counts <- function(df, 
                           direction = "descending",
                           variable = "count"){
  var_symbol <- as.symbol(variable)
  if(direction == "ascending" & variable == "count"){
    dplyr::arrange(df, !!var_symbol)
  } else if(direction == "descending" & variable != "count"){
    dplyr::arrange(df, dplyr::desc(!!var_symbol))
  } else {
    df
  }
}
