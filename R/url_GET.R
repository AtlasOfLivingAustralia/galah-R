# Wrapper for getting data using crul
# tryCatch() used to ensure it is impossible for {galah} to return an error
# because the target atlas is offline.

#' @importFrom glue glue
#' @importFrom rlang inform
url_GET <- function(url, 
                    params = list(), 
                    slot_name = NULL,
                    error_call = caller_env()) {

  cli <- HttpClient$new(
    url = url,
    headers = list("User-Agent" = galah_version_string()))

  # workaround for fq troubles
  if (length(params$fq) > 1) {
    cli$url <- build_fq_url(url, params)
    res <- try(cli$get(), silent = TRUE)
  } else {
    res <- try(cli$get(query = params, encode = "json"), silent = TRUE)
  }
  # print(res$request$url) # uncomment and load package to see url calls

  # handle errors
  # nothing returned
  if(inherits(res, "try-error")){
    return(NULL)
  }else{
    parse_get(res)
  }
}


# parse a returned object from url_GET or similar
parse_get <- function(x, slot_name = NULL, error_call = caller_env()){
  
  # status returned
  if(x$status_code == 200){
    result <- x$parse("UTF-8") |>
              fromJSON(flatten = TRUE)
    if(!is.null(slot_name)){
      result <- result[[slot_name]]
    }
    attr(result, "url") <- x$url
    return(result)
  }else{
    code_number <- x$status_code
    request_url <- x$request$url
    
    inform(glue("Status code {code_number} returned for url {request_url}."))

    if(code_number == 500) {
      inform(c(i = glue("Status code {code_number} usually indicates an incorrect email."),
               i = glue("Is your email address entered correctly?")))
    }
    return(NULL)
  }
}

cache_filename <- function(args, ext) {
  filename <- paste0(digest(sort(args)), ext)
  file.path(getOption("galah_config")$package$cache_directory, filename)
}