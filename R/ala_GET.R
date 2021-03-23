# Wrapper for getting data
#
# Try using crul
ala_GET <- function(url, path, params = list(), on_error = NULL,
                    paginate = FALSE, limit = NULL, page_size = NULL,
                    offset_param = 'offset') {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      "User-Agent" = user_agent_string()
    )
  )
  
  # workaround for fq troubles
  if (length(params$fq) > 1) {
    cli$url <- build_fq_url(url, path, params)
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
      p$get(path = path, query = params, encode = "json")
      res <- p$parse("UTF-8")
      return(res)
    }
    res <- cli$get(path = path, query = params, encode = "json")
  }
  
  if (!is.null(on_error)) {
    if (res$status_code != 200) {
      on_error(res$status_code)
    }
  } else {
    if (res$status_code != 200) {
      stop("Status code ", res$status_code, "returned for url ",
           res$request$url)
    }
  }

  data <- res$parse("UTF-8")
  return(fromJSON(data, flatten = TRUE))
}

cache_filename <- function(args, ext) {
  filename <- paste0(digest(sort(args)), ext)
  file.path(ala_config()$cache_directory, filename)
}

build_fq_url <- function(url, path, params = list()) {
  url <- parse_url(url)
  url$path <- path
  url$query <- params[names(params) != "fq"]
  join_char <- ifelse(length(url$query) > 0, "&fq=", "?fq=")
  fq <- paste(params$fq, collapse = "&fq=")
  paste0(build_url(url), join_char, URLencode(fq))
}
