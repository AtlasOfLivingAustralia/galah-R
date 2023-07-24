#' Internal version of `collect()` for `type = "media"`
#' @param object of class `data_response`, from `compute()`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collect_media_metadata <- function(.data){
  
  # use occurrences system to download data
  occurrences <- collect_occurrences(.data, wait = FALSE)
  
  if(is.null(occurrences) | !inherits(occurrences, "data.frame")){
    abort("Unable to download occurrences with associated media")
  }
  valid_formats <- c("images", "videos", "sounds")
  if(!any(colnames(occurrences) %in% valid_formats)){
    abort("Downloaded records did not contain media fields")
  }
  
  # get media IDs and associated rows
  ids <- get_media_ids(occurrences)

  # run metadata
  urls <- paste0(sub("%7Bid%7D", "", url_lookup("image_metadata")), ids)
  metadata <- media_metadata(urls)
  
  ## Next steps:
  # use tidyr::unnest_longer on `occurrences`, using `present_formats`
  # use dplyr::right_join to connect to format info
  # return
}


#' Internal function to pull out identifiers
#' @noRd
#' @keywords Internal
get_media_ids <- function(df){
  present_formats <- valid_formats[valid_formats %in% colnames(df)]
  ids <- do.call(c, df[, present_formats]) |> 
    unlist()
  ids <- ids[!is.na(ids) & ids != ""]
  names(ids) <- NULL
  ids
}

#' Internal function to get metadata for a list of media ids
#' @noRd
#' @keywords Internal
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
media_metadata <- function(urls){
  verbose <- pour("package", "verbose")
  if (verbose) { pb <- txtProgressBar(max = 1, style = 3) }
  n <- seq_along(urls)
  result <- lapply(n, function(a){
    x <- url_GET(urls[a])
    if (verbose) {
      val <- (a / max(n))
      setTxtProgressBar(pb, val)
    }
    as.data.frame(x[lengths(x) == 1])
  })
  bind_rows(result) |> tibble()
}


## BELOW CODE IS PROBABLY OBSOLETE
old_code_for_collect_media <- function(.data){
  
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
  row_index <- seq_len(nrow(df_long))
  df_long$row <- row_index
  
  # get metadata from the relevant atlas
  ids_vector <- unlist(ids_list)
  names(ids_vector) <- row_index
  ids_vector <- ids_vector[ids_vector != ""]
  metadata <- media_metadata(ids = ids_vector)
  if(is.null(metadata)){
    inform("Calling the metadata API failed for `atlas_media`")
    return(tibble())
  }

  # i.e. service is online, but no data available
  if (nrow(metadata) == 0) {
    if(pour("package", "verbose")){
      system_down_message("search_media")
    }
    return(df_long)
  } 

  # Select only the columns we want
  colnames(metadata) <- rename_columns(names(metadata), type = "media")
  file_ext <- str_extract(metadata$original_file_name, ".[:alpha:]+$")
  metadata <- metadata[colnames(metadata) %in% wanted_columns("media")]
  metadata$file_extension <- file_ext
  metadata$row <- as.integer(names(ids_vector))

  # join image metadata with occurrence data
  all_data <- merge(df_long, metadata, by = "row", all = TRUE)
  tibble(all_data[, -1])
 
}


#' #' Obsolete version of the above
#' #' @param An object of class `data_query` containing fields 'ids' and 'type'
#' #' @keywords Internal
#' #' @noRd
#' collect_media_metadata <- function(.data){
#'   # note: this uses IDs only
#'   # In progress for replacement by `compute_media()`; but this code may be more recent
#'   
#'   verbose <- pour("package", "verbose")
#'   
#'   if(verbose){
#'     inform(glue("Downloading metadata for {length(.data$ids)} files"))
#'   }
#'   metadata <- media_metadata(paste0(.data$url, .data$ids))
#'   
#'   if(is.null(metadata)){
#'     inform("Calling the metadata API failed for `atlas_media`")
#'     return(tibble())
#'   }
#'   
#'   # i.e. service is online, but no data available
#'   if(nrow(metadata) < 1) {
#'     if(verbose){
#'       system_down_message("collect() with type = 'media-metadata'")
#'     }
#'     return(df_long)
#'   } 
#'   
#'   # Select only the columns we want
#'   colnames(metadata) <- rename_columns(names(metadata), type = "media")
#'   file_ext <- str_extract(metadata$original_file_name, ".[:alpha:]+$")
#'   metadata <- metadata[colnames(metadata) %in% wanted_columns("media")]
#'   metadata$file_extension <- file_ext
#'   
#'   tibble(metadata)
#' }