#  pagination based on original code from atlas_GET (<1.5.1)
#' @importFrom glue glue
#' @importFrom rlang inform
url_paginate <- function(url, 
                    params = list(), 
                    group_size, 
                    limit_name = "max",
                    offset_name = "offset",
                    slot_name = NULL,
                    limit = 1000,
                    error_call = caller_env()) {
  
  cli <- HttpClient$new(
    url = url,
    headers = list("User-Agent" = galah_version_string()))

  # workaround for fq troubles
  if (length(params$fq) > 1) {
    cli$url <- build_fq_url(url, params)
    p <- Paginator$new(cli,
                       chunk = group_size,
                       limit,
                       limit_param = limit_name,
                       offset_param = offset_name)
    res <- try(p$get(encode = "json"), silent = TRUE)
  } else {
    p <- Paginator$new(cli,
                       chunk = group_size,
                       limit = limit,
                       limit_param = limit_name,
                       offset_param = offset_name)
    res <- try(p$get(query = params, encode = "json"), silent = TRUE) # note added 'params'
  }

  if(inherits(res, "try-error")){
    return(NULL)
  }else{
    lapply(res, function(a){
      parse_get(a, slot_name = slot_name)}) |>
    bind_rows() |>
    tibble()
  }

}


# Function for paginating over simple APIs
# Note that the error handling here is not as advanced as above,
# may not be production ready
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble

url_paginate_alternative <- function(url, 
                                     group_size, 
                                     limit_name = "max",
                                     slot_name = NULL
){
  
  verbose <- getOption("galah_config")$package$verbose
  
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
    result <- url_GET(url_tr)
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
