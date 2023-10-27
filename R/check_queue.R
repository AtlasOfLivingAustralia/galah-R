#' Internal function to check queue status, with rate limiting
#' @noRd
#' @keywords Internal
check_queue <- function(q_obj, wait = FALSE){
  # process supplied object
  if(q_obj$status == "incomplete"){
    download_response <- c(list(type = q_obj$type),
                           check_occurrence_status(q_obj))
    class(download_response) <- "query"
    if(wait){
      download_response <- c(list(type = q_obj$type),
                             check_queue_loop(q_obj))
      class(download_response) <- "query"
      download_response
    }else{
      download_response
    }
  }else{
    q_obj
  }  
}

#' Internal function to check queue status, with rate limiting
#' @importFrom purrr rate_delay
#' @importFrom purrr rate_sleep
#' @noRd
#' @keywords Internal
check_queue_loop <- function(q_obj){
  rate_object <- set_rate()
  current_queue <- q_obj$queue_size
  continue <- TRUE
  iter <- 1
  verbose <- pour("package", "verbose", .pkg = "galah")
  if(verbose){
    inform(glue("Current queue length: {current_queue}"))
  }
  while(continue == TRUE){
    q_obj <- check_occurrence_status(q_obj)
    continue <- continue_while_loop(q_obj)
    if(continue){    
      iter <- iter + 1
      if(iter > 99){
        inform(c("No data were returned after 100 tries.", 
                 i = "If you have saved this output using e.g. `x <- collect(q_obj)`,", 
                 i = "you can try again later using `collect(x)`"))
        return(q_obj)     
      }else{
        current_queue <- check_queue_size(q_obj, current_queue)
        rate_sleep(rate_object, quiet = verbose)
      }
    }else{
      return(q_obj)
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
check_queue_size <- function(q_obj, current_queue){
  verbose <- pour("package", "verbose", .pkg = "galah")
  if(q_obj$queue_size < current_queue & q_obj$queue_size > 0){
    current_queue <- q_obj$queue_size
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
