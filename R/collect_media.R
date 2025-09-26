#' Internal version of `collect()` for `request_data(type = "media")`
#' @param object of class `data_response`, from `compute()`
#' @noRd
#' @keywords Internal
collect_media_metadata <- function(.query){
  result <- query_API(.query) |>
    purrr::pluck("results") |>
    dplyr::bind_rows()   
  if(nrow(result) < 1){ # case where no data returned
    if(potions::pour("package", "verbose")){
      cli::cli_warn("No data returned from `metadata/media` API")
    }
    ids <- .query$body |>
      jsonlite::fromJSON() |>
      unlist()
    result <- tibble::tibble(image_id = ids)
  }else{
    colnames(result) <- rename_columns(names(result), type = "media")
  }
  # Select only the rows and columns we want 
  result |> 
    dplyr::filter(!is.na(result$image_id)) |>
    dplyr::select(dplyr::any_of(wanted_columns("media")))
}

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

#' Collect media files
#' 
#' @description
#' This function downloads full-sized or thumbnail images and media files 
#' to a local directory using information from [atlas_media()] 
#' 
#' @param df A `tibble` returned by `atlas_media()` or a pipe starting with 
#' `request_data(type = "media")`.
#' @param thumbnail Default is `FALSE`. If `TRUE` will download small 
#' thumbnail-sized images, rather than full size images (default).
#' @param path  
#'    `r lifecycle::badge("deprecated")` 
#'    Use `galah_config(directory = "path-to-directory)"` instead. Supply a path 
#'    to a local folder/directory where downloaded media will be saved to.
#'    
#' @return Invisibly returns a `tibble` listing the number of files downloaded,
#' grouped by their HTML status codes. Primarily called for the side effect of
#' downloading available image & media files to a user local directory.
#' @examples \dontrun{
#' # Use `atlas_media()` to return a `tibble` of records that contain media
#' x <- galah_call() |> 
#'   identify("perameles") |>
#'   filter(year == 2015) |>
#'   atlas_media()
#' 
#' # To download media files, add `collect_media()` to the end of a query 
#' galah_config(directory = "media_files")
#' collect_media(x)
#' 
#' # Since version 2.0, it is possible to run all steps in sequence
#' # first, get occurrences, making sure to include media fields:
#' occurrences_df <- request_data() |>
#'   identify("Regent Honeyeater") |>
#'   filter(!is.na(images), year == 2011) |>
#'   select(group = "media") |>
#'   collect()
#'  
#' # second, get media metadata
#' media_info <- request_metadata() |>
#'   filter(media == occurrences_df) |>
#'   collect()
#'   
#' # the two steps above + `right_join()` are synonymous with `atlas_media()`
#' # third, get images
#' request_files() |>
#'   filter(media == media_info) |>
#'   collect(thumbnail = TRUE)
#' # step three is synonymous with `collect_media()`
#' }
#' @export
collect_media <- function(df, 
                          thumbnail = FALSE, 
                          path
                          ){
  if (!missing(path)) {
    lifecycle::deprecate_stop(
      when = "2.0.0",
      what = "collect_media(path = )",
      details = "Use `galah_config(directory = )` to supply a folder path instead."
    )
  }
  
  # suggest option to set directory in galah_config()
  user_directory <- potions::pour("package", "directory")
  if (stringr::str_detect(user_directory, "Temp")) {
    cli::cli_inform("{cli::col_magenta('To change which file directory media files are saved to, use `galah_config(directory = )`.')}")
  }
  
  request_files() |>
    filter("media" == df) |>
    collapse(thumbnail = thumbnail) |>
    collect()
}
