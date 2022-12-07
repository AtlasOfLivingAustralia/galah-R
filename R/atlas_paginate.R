# Function for paginating over simple APIs
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

atlas_paginate <- function(url, 
                           group_size, 
                           limit_name = "max",
                           slot_name = NULL
                           ){
  
  verbose <- getOption("galah_config")$verbose
  
  # set up loop architecture
  offset_value <- 0 # initial offset, updated per loop
  data_runs <- 0 # how many iterations so far?
  data_size <- group_size # how much data returned in this run?
  data_out <- vector(mode = "list", length = 20) # storage
  
  if(verbose){cat("downloading: ")}
  while(data_runs <= 20 && data_size == group_size){
    # build url
    url_tr <- paste0(url, 
                     "?", limit_name ,"=", group_size,
                     "&offset=", offset_value)
    # get object
    result <- atlas_GET(url_tr)
    if(!is.null(slot_name)){
      result <- result[[slot_name]]
    }
    
    # save out results
    data_runs <- data_runs + 1
    data_out[[data_runs]] <- tibble(result)
    
    # track progress, set up next run
    offset_value <- offset_value + group_size
    data_size <- nrow(result)
    if(verbose){cat(paste0("..", offset_value))}
  }
  
  bind_rows(data_out)
  
}