#' Workhorse function to compute occurrences
#' @noRd
#' @keywords Internal
compute_occurrences <- function(.data){
  if(is_gbif()){
    post_result <- query_API(.data)
    status_code <- list(url = paste0(
                                "https://api.gbif.org/v1/occurrence/download/", 
                                post_result)) |>
                   query_API()
  }else{
    status_code <- query_API(.data) |> # change to check_occurrence_complete()?
                   as.list() |>
                   check_occurrence_response()
    if(pour("package", "verbose")){
      n_records <- status_code$total_records
      inform(glue("Request for {n_records} placed in queue"))
    }
  }
  result <- c(
    list(type = "occurrences"),
    status_code)
  class(result) <- "data_response"
  return(result)
}