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
    if(nrow(result) > 0 &&
       pour("atlas", "region", .pkg = "galah") == "Australia"){
      names(result) <- rename_columns(names(result),
                                      type = "checklist")
      result <- result[, colnames(result) %in% wanted_columns("checklist")]
    }
    result
  }
}