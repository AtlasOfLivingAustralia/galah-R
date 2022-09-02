#' @rdname show_all_minifunctions
#' @export show_all_profiles
show_all_profiles <- function() {
  
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_profiles")
  # url <- server_config("data_quality_base_url") #  this doesn't run a query,
    # but does ping an error if the selected atlas doesn't support profiles
 
  if(update_needed){ # i.e. we'd like to run a query
    # return only enabled profiles?
    resp <- atlas_url("quality_profiles") |> atlas_GET()
    if(is.null(resp)){ # if calling the API fails, return cached data
      df <- galah_internal_cache()$show_all_profiles
      attr(df, "ARCHIVED") <- NULL # remove identifying attributes
    }else{
      df <- as_tibble(resp[wanted_columns(type = "profile")])
      galah_internal_cache(show_all_profiles = df)
    }    
  }else{
     df <- galah_internal_cache()$show_all_profiles
  }
  df
}

#' @rdname search_minifunctions
#' @export search_ranks
search_profiles <- function(query){
  df <- show_all_profiles()
  text_string <- apply(df[, -1], 1, function(a){paste(a, collapse = " ")})
  df[grepl(tolower(query), tolower(text_string)), ]
}