#' Collect counts
#' @importFrom dplyr full_join
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_counts <- function(.data){
  # deal with expand = TRUE separately
  if(length(.data$url) > 1){
    # get data from supplied urls
    result <- lapply(seq_along(.data$url),
                     function(a){
                       data_tr <- .data
                       data_tr$url <- .data$url[[a]]
                       query_API(data_tr) |>
                         pluck(!!!list(1, "fieldResult")) |>
                         bind_rows()
                       # url_parse(data_tr$url)$query
                     })
    # name urls in compute(), so that they can be parsed during collect()
    # this section is incomplete!
    
    # # convert into a tibble, join with label info from `compute()`
    # result_df <- result |> 
    #   bind_rows() |>
    #   tibble() |>
    #   full_join(.data$url, by = "url")
    # 
    # # subset to required columns
    # keep_names <- c(colnames(.data$url), "label", "count")
    # keep_names <- keep_names[-which(keep_names == "url")]
    # subset_df <- result_df[, keep_names]
    # 
    # # rename "label" to correct field name
    # url_field_name <- attributes(.data$url)$url_field
    # names(subset_df)[names(subset_df) == "label"] <- url_field_name
    
    return(result)
  }else{
    result <- query_API(.data)
    if(inherits(result, "data.frame")){
      # subset to required columns
      subset_df <- tibble(result[, c("label", "count")])
      
      # rename "label" to correct field name
      url_field_name <- attributes(.data)$fields
      names(subset_df)[names(subset_df) == "label"] <- url_field_name
      
      return(subset_df)
    }else{
      tibble(count = result)
    }
  }
}