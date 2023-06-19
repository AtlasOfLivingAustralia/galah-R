#' Compute media
#' 
#' Note that this is effectively synonymous with 
#' collect("occurrences", type = "media").
#' 
#' @param .data An object of class `data_query` (from `collapse.data_request()`)
#' @noRd
#' @keywords Internal
compute_media <- function(.data, type){
  # get occurrences with associated metadata
  .data$what <- "occurrences"
  .data$type <- "media"
  resp <- collect(.data) |>
          collect_occurrences_media()
  
  # error catching
  if(nrow(resp) < 1){
    abort("No data returned by compute('media')")
  }
  
  # convert to a list-like format
  result <- list(
    summary = list(
      n = nrow(resp),
      size = format_bytes(sum(resp$size_in_bytes))),
    data = resp[, c("media_id", "file_extension", "image_url", "size_in_bytes")],
    what = "media")
  class(result) <- "data_response"
  result
}

#' Internal function for formatting file sizes
#' @param x integer number of bytes
#' @noRd
#' @keywords Internal
format_bytes <- function(x){
  x_units <- cut(log10(x), 
               breaks = c(0, 3, 6, 9, 12, Inf), 
               labels = c("B", "KB", "MB", "GB", "TB")) |>
             as.character()
  x_scaled <- switch(x_units,
         "B" = x,
         "KB" = x * 10^-3,
         "MB" = x * 10^-6,
         "GB" = x * 10^-9,
         "TB" = x * 10^-12)
  paste(
    round(x_scaled, digits = 2),
    x_units,
    sep = " ")
}