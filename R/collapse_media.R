#' Internal function to get media data
#' @params .data An object of class `data_request` (from `galah_call()`)
#' @importFrom dplyr filter
#' @noRd
#' @keywords Internal
collapse_media <- function(.data){

  # overwrite earlier `select` args to only allow minimal fields
  lower_types <- paste0(tolower(.data$type), "s")
  
  # overwrite `select` functions to only return required information
  # NOTE: order matters here; if recordID is not first, no data are returned
  .data$select <- galah_select(
    "recordID", 
    # group = "media") |>
    group = c("basic", "media")) |>
    filter(name %in% c("recordID", "multimedia", lower_types))
  
  # pass to collapse_occurrences for query construction
  result <- collapse_occurrences(.data)
  
  # filter to records that contain media of requested types 
  # NOTE: Might be more efficient to use `filter` for this, as it 
  # includes to remove duplicated rows
  media_fq <- glue("({lower_types}:*)") |>
              glue_collapse(" OR ")
  result$query$fq <- glue_collapse(c(result$query$fq, media_fq), " AND ") |>
                     as.character()
  
  # convert to correct type and return
  result$what <- "media"
  return(result)
}