#' workhorse function to get species lists from an atlas
#' @keywords Internal
#' @noRd
collect_species <- function(.data, file = NULL){
  if(is_gbif()){
    collect_occurrences(.data)
  }else{
    .data$file <- check_download_filename(file, ext = "csv")
    query_API(.data)
    result <- read_csv(.data$file, col_types = cols()) # NOTE: used to have tryCatch()
    if(nrow(result) > 0 && 
       pour("atlas", "region", .pkg = "galah") == "Australia"){
      names(result) <- rename_columns(names(result), 
                                      type = "checklist")
      result <- result[, wanted_columns("checklist")]
    }
    return(result)
  }
}