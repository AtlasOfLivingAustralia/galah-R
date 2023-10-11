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
      if(is_gbif()){
        abort("Not coded yet lol") # FIXME
      }else{
        download_response <- c(list(type = .data$type),
                               check_queue_LA(.data))
        class(download_response) <- "query"
        download_response
      }
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
check_queue_LA <- function(.data){
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
  if(is.null(.data$queue_size)){
    .data$queue_size <- 0
  }
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

#' Internal function to check download queue for GBIF
#' NOT UPDATED YET
#' @noRd
#' @keywords Internal
check_queue_GBIF <- function(url){
  
  verbose <- pour("package", "verbose")
  if(verbose){inform("Checking queue")}
  result <- url_GET(url)
  if(verbose){
    current_status <- print_status(result, "")
  }
  
  if(is.null(result)){
    return(NULL)
  }else{
    interval_times <- api_intervals()
    n_intervals <- length(interval_times)
    iter <- 1
    continue <- continue_while_loop(result, success_tag = "SUCCEEDED")
    while(continue == TRUE){
      # query
      if(verbose){
        current_status <- print_status(result, current_status)
      }
      result <- url_GET(url)
      continue <- continue_while_loop(result, success_tag = "SUCCEEDED")
      if(continue){
        # pause
        if(iter <= n_intervals){
          lag <- interval_times[iter]
        }else{
          lag <- 60
        }
        Sys.sleep(lag)
        # iterate
        iter <- iter + 1
      }else{
        if(verbose){inform("succeeded")}
        break
      }
    }
  }

  if(any(names(result) == "downloadLink")){  
    x <- result$downloadLink
    attr(x, "doi") <- result$doi
    return(x)
  }else{
    NULL
  }
}

# status of a gbif call
print_status <- function(result, current_status){
  if(any(names(result) == "status")){
    if(result$status != current_status){
      current_status <- result$status
      cat(paste0(" ", tolower(current_status), " "))
    }else{
      cat(".")
    }
  }else{
    cat(".")
  }
  return(current_status)
}