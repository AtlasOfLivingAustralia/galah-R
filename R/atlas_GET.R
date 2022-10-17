# Wrapper for getting data using crul
# tryCatch() used to ensure it is impossible for {galah} to return an error
# because the target atlas is offline.


atlas_GET <- function(url, 
                    params = list(), on_error = NULL,
                    paginate = FALSE, limit = NULL, page_size = NULL,
                    offset_param = 'offset') {
   tryCatch({
     internal_GET(
       url = url,
       params = params,
       on_error = on_error,
       paginate = paginate,
       limit = limit,
       page_size = page_size,
       offset_param = offset_param)
     }, 
     error = function(a){return(NULL)},
     warning = function(a){return(NULL)}
   )
}

internal_GET <- function(url, params = list(), on_error = NULL,
                    paginate = FALSE, limit = NULL, page_size = NULL,
                    offset_param = 'offset',
                    error_call = caller_env()) {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      "User-Agent" = galah_version_string()
    )
  )

  # workaround for fq troubles
  if (length(params$fq) > 1) {
    cli$url <- build_fq_url(url, params) # path
    if (paginate) {
      p <- Paginator$new(cli, chunk = page_size, limit_param = 'flimit',
                         offset_param = offset_param, limit = limit)
      p$get()
      res <- p$parse("UTF-8")
      return(res)
    }
    res <- cli$get()
  } else {
    cli$url <- url
    if (paginate) {
      p <- Paginator$new(cli, chunk = page_size, limit_param = 'flimit',
                         offset_param = offset_param, limit = limit)
      p$get(query = params, encode = "json") # path = path,
      res <- p$parse("UTF-8")
      return(res)
    }
    res <- cli$get(query = params, encode = "json") #path = path, 
  }
  # print(res$request$url) # uncomment and load package to see url calls
  if (!is.null(on_error)) {
    if (res$status_code != 200) {
      on_error(res$status_code)
    }
  } else {
    if (res$status_code != 200) {
      code_number <- res$status_code
      request_url <- res$request$url
      abort(glue("Status code {code_number} returned for url {request_url}."),
            call = error_call)
    }
  }

  data <- res$parse("UTF-8")
  result <- fromJSON(data, flatten = TRUE)
  attr(result, "url") <- res$url
  return(result)
}

cache_filename <- function(args, ext) {
  filename <- paste0(digest(sort(args)), ext)
  file.path(getOption("galah_config")$cache_directory, filename)
}