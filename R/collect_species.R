#' workhorse function to get species lists from an atlas
#' @keywords Internal
#' @noRd
collect_species <- function(.data){
  if(is_gbif()){
    collect_occurrences(.data)
  }else{
    tmp <- tempfile()
    result <- url_download(.data$url, 
                           params = .data$query, 
                           cache_file = tmp, 
                           ext = "csv")
    if(is.null(result)){
      system_down_message("atlas_species")
      return(tibble())
    }else{
      if(nrow(result) > 0 && pour("atlas", "region") == "Australia"){
        # overwrite file with fixed names
        names(result) <- rename_columns(names(result), type = "checklist")
        result <- result[, wanted_columns("checklist")]
      }
      return(result |> tibble())
    }
  }
}

#' Internal function to load a csv file downloaded by `query_API()`
#' NOTE: THIS IS PORTED FROM `url_download()` and HAS NOT BEEN UPDATED OR TESTED
#' @noRd
#' @keywords Internal
load_csv <- function(.data){
  tryCatch(
    read_csv(res$content, col_types = cols()),
    error = function(e) {
      e$message <- inform("No species matching the supplied filters were found.")
      close(file(cache_file))
      unlink(cache_file)
      stop(e)
    }
  )
}