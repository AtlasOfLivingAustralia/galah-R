#' Internal function to get media data
#' @noRd
#' @keywords Internal
collapse_media <- function(.data){

  # overwrite earlier `select` args to only allow minimal fields
  lower_types <- paste0(tolower(.data$type), "s")
  .data$select <- galah_select(group = "media") |>
    filter(name %in% lower_types | name == "multimedia") 
  
  # filter to records that contain media of requested types 
  .data <- .data |> galah_filter('multimedia' %in% lower_types) 
  
  # pass to collapse_occurrences for query construction
  result <- collapse_occurrences(.data)
  result$what <- "media"
  return(result)
}