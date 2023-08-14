#' Internal version of `collect()` for `request_data(type = "media")`
#' @param object of class `data_response`, from `compute()`
#' @importFrom dplyr all_of
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collect_media_metadata <- function(.data){
  result <- query_API(.data)

  ## Error catching  
  # if(is.null(metadata)){
  #   inform("Calling the metadata API failed for `atlas_media`")
  #   return(tibble())
  # }
  # 
  # # i.e. service is online, but no data available
  # if (nrow(metadata) == 0) {
  #   if(pour("package", "verbose")){
  #     system_down_message("search_media")
  #   }
  #   return(df_long)
  # } 
  
  # # Select only the columns we want (tidyverse syntax?)
  colnames(result) <- rename_columns(names(result), type = "media")
  
  # clean up and return
  result |> 
    # tidyr::drop_na(image_url) |> # may need to filter for missingness elsewhere too
    filter(!is.na(media_id)) |>
    select(all_of(wanted_columns("media")))
}

#' Internal version of `collect()` for `request_files(type = "media")`
#' @param object of class `files_response`, from `compute()`
#' @noRd
#' @keywords Internal
collect_media_files <- function(.data){
  
  # up to here
}