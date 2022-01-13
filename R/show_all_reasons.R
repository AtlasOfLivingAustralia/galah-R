#' List valid download reasons
#'
#' When downloading occurrence data with [atlas_occurrences()] the
#' ALA APIs require a reason for download to be specified. By default, a
#' download reason of 'scientific research' is set for you, but if you wish to
#' change this you can do so with [galah_config()]. Use this function
#' to view the list of download reason code and names. When specifying a reason,
#' you can use either the download code or name.
#' @rdname show_all_reasons
#' @seealso This function is helpful in setting up [galah_config()].
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) of 
#' valid download reasons, containing the id and name for each reason.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' Show a listing of all accepted reasons for downloading occurrence data
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_reasons()
#' ```
#' 
#' Add your download reason when configuring your session with [galah_config()]
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(download_reason_id = 3)
#' ```
#' 
#' @export
show_all_reasons <- function() {
   
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_reasons")
  atlas <- getOption("galah_config")$atlas
  url <- server_config("logger_base_url")
  
  if(update_needed){ # i.e. we'd like to run a query
    ## return list of valid "reasons for use" codes
    out <- atlas_GET(url, path = "service/logger/reasons")  
    if(is.null(out)){
      # if cached values reflect the correct atlas, return requested info
      if(attr(df, "atlas_name") == atlas){ 
        df <- galah_internal_cache()$show_all_reasons
        attr(df, "ARCHIVED") <- NULL # remove identifying attributes
        return(df)
      # otherwise return a message
      }else{ 
        bullets <- c(
          "Calling the API failed for `show_all_reasons`.",
          i = "This might mean that the system is down."
        )
        inform(bullets)
        return(tibble())
      }
    # if the API call works
    }else{
      if (any(names(out) == "deprecated")) out <- out[!out$deprecated, ]
      out <- out[wanted_columns("reasons")]
      # sort by id to make it less confusing
      row.names(out) <- out$id
      df <- as_tibble(out[order(out$id), ])
      attr(df, "atlas_name") <- atlas
      return(df)
    }
  # if no update is needed
  }else{
    return(galah_internal_cache()$show_all_reasons)
  }
}