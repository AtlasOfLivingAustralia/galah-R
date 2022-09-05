#' @rdname show_all_minifunctions
#' @export
show_all_reasons <- function() {
   
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_reasons")
  atlas <- getOption("galah_config")$atlas
  
  if(update_needed){ # i.e. we'd like to run a query
    ## return list of valid "reasons for use" codes
    out <- atlas_url("logger_reasons") |> atlas_GET()
    if(is.null(out)){
      df <- galah_internal_cache()$show_all_reasons
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

#' @rdname search_minifunctions
#' @export search_reasons
search_reasons <- function(query){
  df <- show_all_reasons()
  df[grepl(tolower(query), tolower(df$name)), ]
}