#' workhorse function to get species lists from an atlas
#' @keywords Internal
#' @noRd
collect_species <- function(q_obj, file = NULL){
  if(is_gbif()){
    collect_occurrences(q_obj, wait = TRUE)
  }else{
    q_obj$file <- check_download_filename(file, ext = "csv")
    query_API(q_obj)
    result <- read_csv(q_obj$file, col_types = cols()) # NOTE: used to have tryCatch()
    if(nrow(result) > 0 && 
       pour("atlas", "region", .pkg = "galah") == "Australia"){
      names(result) <- rename_columns(names(result), 
                                      type = "checklist")
      result <- result[, wanted_columns("checklist")]
    }
    result
  }
}
