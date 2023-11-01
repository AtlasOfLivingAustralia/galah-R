#' Internal function to check queue status, with rate limiting
#' @noRd
#' @keywords Internal
check_queue <- function(.query, wait = FALSE){
  # process supplied object
  if(.query$status == "incomplete"){
    download_response <- c(list(type = .query$type),
                           check_occurrence_status(.query))
    class(download_response) <- "query"
    if(wait){
      download_response <- c(list(type = .query$type),
                             check_queue_loop(.query))
      class(download_response) <- "query"
      download_response
    }else{
      download_response
    }
  }else{
    .query
  }  
}

#' Internal function to check queue status, with rate limiting
#' @importFrom purrr rate_delay
#' @importFrom purrr rate_sleep
#' @noRd
#' @keywords Internal
check_queue_loop <- function(.query){
  rate_object <- set_rate()
  current_queue <- .query$queue_size
  continue <- TRUE
  iter <- 1
  verbose <- pour("package", "verbose", .pkg = "galah")
  if(verbose){
    inform(glue("Current queue length: {current_queue}"))
  }
  while(continue == TRUE){
    .query <- check_occurrence_status(.query)
    continue <- continue_while_loop(.query)
    if(continue){    
      iter <- iter + 1
      if(iter > 99){
        inform(c("No data were returned after 100 tries.", 
                 i = "If you have saved this output using e.g. `x <- collect(.query)`,", 
                 i = "you can try again later using `collect(x)`"))
        return(.query)     
      }else{
        current_queue <- check_queue_size(.query, current_queue)
        rate_sleep(rate_object, quiet = verbose)
      }
    }else{
      return(.query)
    }
  }
}

#' Internal function for rate limiting
#' @importFrom purrr rate_backoff
#' @noRd
#' @keywords Internal
set_rate <- function(){
  rate_backoff(pause_base = 0.5, 
               pause_cap = 60, 
               max_times = 100,
               jitter = FALSE)
}

#' Internal function to check queue size
#' @noRd
#' @keywords Internal
check_queue_size <- function(.query, current_queue){
  verbose <- pour("package", "verbose", .pkg = "galah")
  if(.query$queue_size < current_queue & .query$queue_size > 0){
    current_queue <- .query$queue_size
    if(verbose){
      inform(glue("Queue length: {current_queue}"))
    }
  }else{
    if(verbose){cat("-")}
  }
  current_queue
}

#' Internal function to determine whether to keep looping
#' @noRd
#' @keywords Internal
continue_while_loop <- function(x, iter){
  z <- TRUE
  if(!is.null(x)){
    if(any(names(x) == "status")){
      if(x$status == "complete"){
        z <- FALSE
      }
    }
  }
  return(z)
}
