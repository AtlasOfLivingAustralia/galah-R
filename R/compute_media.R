compute_media <- function(.data){
  occ <- .data$"data/occurrences"
  media_cols <- which(colnames(occ) %in% c("images", "video", "sounds"))
  media_ids <- do.call(c, occ[, media_cols]) |>
    unlist()
  names(media_ids) <- NULL
  .data$body <- list(mediaIds = media_ids) |> toJSON()
  .data
}