#' workhorse function to get species lists from an atlas
#' @keywords Internal
#' @noRd
collect_species <- function(.query, file = NULL){
  if(is_gbif()){
    collect_occurrences(.query, wait = TRUE)
  }else{
    .query$file <- check_download_filename(file, ext = "csv")
    query_API(.query)
    result <- read_csv(.query$file, col_types = cols()) # NOTE: used to have tryCatch()
    if(nrow(result) > 0){
      names(result) <- names(result) |>
                       rename_columns(type = "checklist")
    }
    result
  }
}