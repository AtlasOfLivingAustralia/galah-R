#' Collect counts
#' @importFrom dplyr full_join
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_counts <- function(.data){
  
  # deal with expand = TRUE separately
  if(inherits(.data$url, "data.frame")){
    # get data from supplied urls
    result <- lapply(.data$url$url, function(a, col){
      x <- url_GET(a)[[col]][[1]]
      x$url <- a
      return(x)},
      col = .data$column)
    
    # convert into a tibble, join with label info from `compute()`
    result_df <- result |> 
      bind_rows() |>
      tibble() |>
      full_join(.data$url, by = "url")
    
    # subset to required columns
    keep_names <- c(colnames(.data$url), "label", "count")
    keep_names <- keep_names[-which(keep_names == "url")]
    return(result_df[, keep_names])
  }else{
    result <- url_GET(.data$url)[[.data$column]][[1]]
    if(inherits(result, "data.frame")){
      tibble(result[, c("label", "count")])
    }else{
      result
    }
  }
}