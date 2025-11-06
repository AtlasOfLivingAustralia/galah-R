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
    cli::col_magenta("To change which file directory media files are saved to, use `galah_config(directory = )`.") |>
    cli::cli_text()
  }
  
  request_files() |>
    filter("media" == df) |>
    collapse(thumbnail = thumbnail) |>
    collect()
}
