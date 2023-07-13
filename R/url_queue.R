#' Join queue to wait for download
#' @noRd
#' @keywords internal 
#' @importFrom potions pour
url_queue <- function(status_initial) {

  # check running status, with rate limiting
  status_post_queue <- check_queue(status_initial)
  if(!is.null(status_post_queue$status)){
    if(status_post_queue$status != "finished"){
      status <- check_running(status_post_queue)
    }else{
      status <- status_post_queue
    }
  }else{
    status <- NULL
  }

  return(status$downloadUrl)
}

# set API intervals
api_intervals <- function(){
  c(
    rep(0.5, 4),
    rep(1, 3),
    rep(2, 5),
    rep(5, 8),
    rep(10, 10),
    rep(30, 10))
}

# check queue status, with rate limiting
check_queue <- function(status_initial){
  interval_times <- api_intervals()
  n_intervals <- length(interval_times)
  status <- status_initial
  iter <- 1
  queue_size <- status$queueSize
  continue <- continue_while_loop(status, success_tag = "finished")
  
  verbose <- pour("package", "verbose")
  if(verbose){
    inform(c("Checking queue", glue("Current queue length: {queue_size}")))
    current_status <- ""
  }
  
  while(continue == TRUE){
    if(verbose){
      if(is.null(status$queueSize)){status$queueSize <- 0}
      if(status$queueSize < queue_size & status$queueSize > 0){
        queue_size <- status$queueSize
        inform(glue("Queue length: {queue_size}"))
      }else{
        current_status <- print_status(status, current_status)
      }
    }
    status <- url_GET(status$statusUrl)
    continue <- continue_while_loop(status, success_tag = "finished")
    if(continue){
      if(iter <= n_intervals){
        lag <- interval_times[iter]
      }else{
        lag <- 60
      }
      Sys.sleep(lag)
    }
    iter <- iter + 1
  }
  return(status)
}

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

# several checks to determine whether to keep looping
continue_while_loop <- function(x, success_tag){
  z <- TRUE
  if(!is.null(x)){
    if(any(names(x) == "status")){
      if(x$status == success_tag){
        z <- FALSE
      }
    }
  }
  return(z)
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

check_running <- function(status){
  interval_times <- api_intervals()
  n_intervals <- length(interval_times)
  iter <- 1
  
  verbose <- pour("package", "verbose")
  if(verbose){
    atlas_org <- pour("atlas", "organisation")
    inform(glue("Sending query to {atlas_org}"))
    pb <- txtProgressBar(max = 1, style = 3)
  }
  
  while(status$status == "running"){
    if(verbose){
      if(is.null(status$records)){status$records <- 0}
      if(is.null(status$totalRecords)){status$totalRecords <- 1}
      setTxtProgressBar(pb, status$records/status$totalRecords)
    }
    status <- url_GET(status$statusUrl)
    if(is.null(status$statusUrl)){
      break()
    }else{
      if(iter <= n_intervals){
        lag <- interval_times[iter]
      }else{
        lag <- 60
      }
      Sys.sleep(lag)
    }
    iter <- iter + 1
  }
  
  if(verbose){
    setTxtProgressBar(pb, value = 1)
    close(pb)
  }
  return(status)
}