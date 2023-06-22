#' Workhorse function to compute occurrences
#' @noRd
#' @keywords Internal
compute_occurrences <- function(.data){
  if(is_gbif()){
    post_result <- url_POST(.data$url,
                            headers = .data$headers,
                            opts = .data$opts,
                            body = .data$body)
    status_code <- paste0("https://api.gbif.org/v1/occurrence/download/", 
                          post_result) |>
      url_GET()
  }else{
    # return n records of query, remove count query info
    # .data$query_n <- .data$count_query |> compute_counts() |> collect_counts()
    # .data <- .data[-which(names(.data) == "count_query")]
    status_code <- url_GET(.data$url, 
                           params = .data$query)
  }
  class(status_code) <- "data_response"
  status_code$type <- "occurrences"
  return(status_code)
}