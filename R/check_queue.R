#' Internal function to check queue status, with rate limiting
#' @noRd
#' @keywords Internal
check_queue <- function(.data, wait = FALSE){
  # process supplied object
  if(.data$status == "incomplete"){
    download_response <- c(list(type = .data$type),
                           check_occurrence_status(.data))
    class(download_response) <- "query"
    if(wait){
      download_response <- c(list(type = .data$type),
                             check_queue_loop(.data))
      class(download_response) <- "query"
      download_response
    }else{
      download_response
    }
  }else{
    .data
  }  
}

#' Internal function to check queue status, with rate limiting
#' @importFrom purrr rate_delay
#' @importFrom purrr rate_sleep
#' @noRd
#' @keywords Internal
check_queue_loop <- function(.data){
  rate_object <- set_rate()
  current_queue <- .data$queue_size
  continue <- TRUE
  iter <- 1
  verbose <- pour("package", "verbose", .pkg = "galah")
  if(verbose){
    inform(glue("Current queue length: {current_queue}"))
  }
  while(continue == TRUE){
    .data <- check_occurrence_status(.data)
    continue <- continue_while_loop(.data)
    if(continue){    
      iter <- iter + 1
      if(iter > 99){
        inform(c("No data were returned after 100 tries.", 
                 i = "If you have saved this output using e.g. `x <- collect(.data)`,", 
                 i = "you can try again later using `collect(x)`"))
        return(.data)     
      }else{
        current_queue <- check_queue_size(.data, current_queue)
        rate_sleep(rate_object, quiet = verbose)
      }
    }else{
      return(.data)
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
check_queue_size <- function(.data, current_queue){
  verbose <- pour("package", "verbose", .pkg = "galah")
  if(.data$queue_size < current_queue & .data$queue_size > 0){
    current_queue <- .data$queue_size
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