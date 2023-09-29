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
collect_media <- function(.data){
  # query API
  result <- query_API(.data) |>
    pluck("results") |>
    bind_rows()
  # Select only the rows and columns we want 
  colnames(result) <- rename_columns(names(result), type = "media")
  result <- result |> 
    # tidyr::drop_na(image_url) |> # may need to filter for missingness elsewhere too
    filter(!is.na(image_id)) |>
    select(any_of(wanted_columns("media")))
  # return merged occurrences and media data
  .data[["data/occurrences"]] |>
    unnest_longer(col = images) |>
    right_join(result, by = c("images" = "image_id"))
}

#' Internal version of `collect()` for `request_files(type = "media")`
#' @param object of class `files_response`, from `compute()`
#' @noRd
#' @keywords Internal
collect_media_files <- function(.data){
  
  # not coded yet
}