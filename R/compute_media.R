#' Compute media
#' @noRd
#' @keywords Internal
compute_media <- function(.data, type){

  # collect occurrences
  .data$what <- "occurrences"
  result <- collect(.data, type = "media")
  
  output <- get_media_ids(result) 
  class(output) <- "data_response"
}

#' Internal function to parse identifiers
#' @noRd
#' @keywords Internal
get_media_ids <- function(df){
  # run checks that 1. a df is supplied, 2. it contains requisite columns
  assert_that(inherits(df, "data.frame"))
  default_cols <- c("images", "videos", "sounds")
  if(!any(colnames(df) %in% default_cols)){
    return(df)
  }
  
  # convert multi-column IDs to single-column
  cols <- df[, colnames(df) %in% default_cols]
  if(ncol(cols) > 1){
    ids <- apply(cols, 1, function(a){
      valid_check <- (a != "") & !is.na(a)
      if(any(valid_check)){
        paste(a[valid_check], collapse = " | ")
      }else{
        ""
      }
    })
  }else{
    ids <- cols[[1]]
  }
  
  # split strings
  ids_list <- str_split(ids, pattern = "\\s\\|\\s")
  
  # create long-form version of non-media rows
  df_long <- df[
    rep(seq_len(nrow(df)), lengths(ids_list)),
    !(colnames(df) %in% default_cols)]
  df_long$row <- seq_len(nrow(df_long))
  
  df_long
}
