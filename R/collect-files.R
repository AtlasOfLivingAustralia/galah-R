#' Internal version of `collect()` for `request_files(type = "media")`
#' @param object of class `files_response`, from `compute()`
#' @noRd
#' @keywords Internal
collect_media_files <- function(.query){
  result <- query_API(.query)
  status_values <- purrr::map(result,
                              \(a){a$status_code}) |>
    unlist()
  result_summary <- tibble::tibble(status_code = status_values) |>
    dplyr::group_by(.data$status_code) |>
    dplyr::count()
  
  # successful downloads
  success <- result_summary |> 
    dplyr::filter(.data$status_code == 200) 
  n_downloaded <- success[["n"]]
  
  # failed downloads
  fail <- NULL
  if(nrow(result_summary) > 1) {
    if(any(result_summary$status_code %in% "403")) {
      fail <- result_summary |>
        dplyr::filter(.data$status_code == 403)
      n_failed <- fail[["n"]]
    }
  }
  user_directory <- potions::pour("package", "directory")
  bullets <- c(
    "v" = "Downloaded {n_downloaded} files successfully (status 200).",
    ">" = "Files saved in local directory: \"{user_directory}\"."
  )
  if(!is.null(fail)) {
    bullets <- c(bullets,
                 "x" = "Failed {n_failed} downloads due to missing images (status 403)")
  }
  cli::cli_inform(bullets)
  invisible(result_summary)
}