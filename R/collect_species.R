#' workhorse function to get species lists from an atlas
#' @keywords Internal
#' @noRd
collect_species <- function(.query, file = NULL){
  if(is_gbif()){
    collect_occurrences(.query, wait = TRUE)
  }else{
    .query$file <- check_download_filename(file, ext = "csv")
    query_API(.query)
    readr::read_csv(.query$file,
                    col_names = get_clean_colnames(.query$file,
                                                   facet = .query$group_by$name),
                    col_types = readr::cols(),
                    skip = 1)
  }
}

#' Internal function to get column names cleanly
#' @keywords Internal
#' @noRd
get_clean_colnames <- function(file, facet){
  column_names <- scan(file,
                       what = character(), 
                       sep = ",",
                       nlines = 1L,
                       quiet = TRUE)
  if(length(column_names) > 0){
    column_names <- camel_to_snake_case(column_names)
    if(grepl("ID$", facet)){
      column_names[1] <- "taxon_concept_id"
    }
    column_names[column_names %in% c("counts", "number_of_records")] <- "count"
    column_names    
  }else{
    TRUE
  }
}