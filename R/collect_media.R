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
collect_media_metadata <- function(.query){
  # query API
  result <- query_API(.query) |>
    pluck("results") |>
    bind_rows()
  # Select only the rows and columns we want 
  colnames(result) <- rename_columns(names(result), type = "media")
  result |> 
    filter(!is.na(result$image_id)) |>
    select(any_of(wanted_columns("media")))
}

#' Internal version of `collect()` for `request_files(type = "media")`
#' @param object of class `files_response`, from `compute()`
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom rlang .data
#' @noRd
#' @keywords Internal
collect_media_files <- function(.query){
  result <- query_API(.query)
  result_summary <- tibble(
    status_code = unlist(lapply(result, function(a){a$status_code}))) |>
    group_by(.data$status_code) |>
    count()
  success <- result_summary |> 
    dplyr::filter(result_summary$status_code == 200) 
  n_downloaded <- success[["n"]]
  user_directory <- pour("package", "directory")
  bullets <- c(
    "v" = glue("Downloaded {n_downloaded} files successfully (status 200)."),
    ">" = glue("Files saved in local directory: \"{user_directory}\".")
  )
  inform(bullets)
  invisible(result_summary)
}

#' Collect media files
#'
#' This function downloads full-sized or thumbnail images and media files using 
#' information from `atlas_media` to a local directory. 
#'
#' @param df `tibble`: returned by `atlas_media()` or a pipe starting with 
#' `request_data(type = "media")`.
#' @param thumbnail `logical`: If `TRUE` will download small thumbnail-sized 
#' images, rather than full size images (default).
#' @param path `string`: 
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
#'   galah_identify("perameles") |>
#'   galah_filter(year == 2015) |>
#'   atlas_media()
#' 
#' # To download media files, add `collect_media()` to the end of a query 
#' galah_config(directory = "media_files")
#' collect_media(x)
#' }
#' @importFrom cli col_magenta
#' @importFrom cli cli_text
#' @importFrom rlang .data
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
  user_directory <- pour("package", "directory")
  if (stringr::str_detect(user_directory, "Temp")) {
    inform(
      cli::cli_text("{cli::col_magenta('To change which file directory media files are saved, use `galah_config(directory = )`.')}")
      )
  }
  
  request_files() |>
    filter("media" == df) |>
    collapse(thumbnail = thumbnail) |>
    collect()
}
