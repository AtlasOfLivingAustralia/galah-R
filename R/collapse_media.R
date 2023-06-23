#' Internal version of `collapse()` for `type = "media"`
#' @param .data An object of class `data_request` (from `galah_call()`)
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @noRd
#' @keywords Internal
collapse_media <- function(.data){
  
  valid_formats <- c("images", "videos", "sounds")
  
  # ensure media columns are present
  if(is.null(.data$select)){
    .data <- update_galah_call(.data, 
                               select = galah_select(group = c("basic", "media")))
  }else{
    if(!any(.data$select$name %in% valid_formats)){
      .data <- update_galah_call(.data, 
                                 select = galah_select(group = "media"))
    }
  }
  
  # use collapse_occurrences to generate query
  result <- collapse_occurrences(.data)
  result$type <- "media"
  
  # filter to records that contain media of requested types 
  # NOTE: Might be more efficient to use `filter` for this, as it 
  # includes code to remove duplicated rows
  present_formats <- valid_formats[valid_formats %in% .data$select$name]
  media_fq <- glue("({present_formats}:*)") |>
    glue_collapse(" OR ")
  media_fq <- glue("({media_fq})")
  if(nchar(result$query$fq) < 1){
    result$query$fq <- as.character(media_fq)
  }else{
    result$query$fq <- glue_collapse(
      c(result$query$fq, media_fq), 
      " AND ") |>
     as.character()
  }
  return(result)
}