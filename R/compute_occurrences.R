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
    status_code <- url_GET(.data$url, 
                           params = .data$query)
  }
  class(status_code) <- "data_response"
  attr(status_code, "type") <- "occurrences"
  return(status_code)
}