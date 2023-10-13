#' Internal version of `collect()` for `request_data(type = "media")`
#' @param object of class `data_response`, from `compute()`
#' @importFrom dplyr any_of
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collect_media_metadata <- function(.data){
  # query API
  result <- query_API(.data) |>
    pluck("results") |>
    bind_rows()
  # Select only the rows and columns we want 
  colnames(result) <- rename_columns(names(result), type = "media")
  result |> 
    filter(!is.na(image_id)) |>
    select(any_of(wanted_columns("media")))
}

#' Internal version of `collect()` for `request_files(type = "media")`
#' @param object of class `files_response`, from `compute()`
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @noRd
#' @keywords Internal
collect_media_files <- function(.data){
  result <- query_API(.data)
  result_summary <- tibble(
    status_code = unlist(lapply(result, function(a){a$status_code}))) |>
    group_by(status_code) |>
    count()
  result_summary
}

#' Collect media files
#'
#' This function downloads full-sized or thumbnail images and media files using 
#' information from `atlas_media` to a local directory. 
#'
#' @param df tibble returned by `atlas_media()` or a pipe starting with 
#' `request_data(type = "media")`
#' @param thumbnail logical: should the download return thumbnails (TRUE) or 
#' full size images (FALSE, the default)
#' @return Available image & media files downloaded to a user local directory.
#' @examples 
#' \dontrun{
#' # Use `atlas_media()` to return a `tibble` of records that contain media
#' x <- galah_call() |> 
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2015) |>
#'   atlas_media()
#' 
#' # To download media files, add `collect_media()` to the end of a query 
#' galah_config(directory = "media_files")
#' collect_media(x)
#' }
#' @export
collect_media <- function(df, thumbnail = FALSE){
  request_files() |>
    filter(media == df) |>
    collapse(thumbnail = thumbnail) |>
    collect()
}