# Internal functions to store objects generated from some `request_metadata` calls
# This increases speed by ensuring that the atlas is only queried when needed.

#' Internal function to overwrite a cached list in a specified place
#' @param x list
#' @noRd
#' @keywords Internal
overwrite_cache <- function(x){
  result <- list(x)
  names(result) <- "check_internal_cache"
  options(result)
}

#' Internal function to reset cache to package defaults
#' @noRd
#' @keywords Internal
reset_cache <- function(){
  overwrite_cache(galah_internal_cached)
}

#' Internal function to add a slot to current cache
#' @noRd
#' @keywords Internal
update_cache <- function(...){
  dots <- list(...)
  if(length(dots) < 1){
    cli::cli_abort("cache not updated")
  }
  result <- retrieve_cache()
  for(i in seq_along(dots)){
    result[[names(dots)[i]]] <- dots[[i]]
  }
  overwrite_cache(result)
}

#' Internal function to retrieve current state of the cache
#' @noRd
#' @keywords Internal
retrieve_cache <- function(slot_name){
  full_cache <- getOption("check_internal_cache")
  if(missing(slot_name)){
    full_cache
  }else{
    if(any(names(full_cache) == slot_name)){
      full_cache[[slot_name]]
    }else{
      NULL
    }
  }
}

#' Internal function to decide whether to update the internal cache
#' @noRd
#' @keywords Internal
check_if_cache_update_needed <- function(function_name){
  df <- retrieve_cache(function_name)
  is_local <- !is.null(attr(df, "ARCHIVED"))
  is_wrong_atlas <- attr(df, "region") != potions::pour("atlas", "region")
  is_too_short <- nrow(df) < 10
  result <- is_local | is_wrong_atlas | is_too_short # if any, update is needed
  if(length(result) < 1){ # bug catcher
    result <- TRUE
  }
  result
}