# Wrapper for getting data
# 
# NOTE: this is only used for caching long queries with ALA:
# atlas_ functions > build_query > cached_query > url_POST
# this functionality is currently disabled
# Ergo this needs checking as to whether we should retain this function

url_POST <- function(url, path, body = list(), encode = "form") {
  tryCatch({
    internal_POST(
      url = url,
      path = path,
      body = body,
      encode = encode)
    }, 
    error = function(a){return(NULL)},
    warning = function(a){return(NULL)}
  )  
}

internal_POST <- function(url, path, body, encode, error_call = caller_env()) {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      "User-Agent" = galah_version_string()
    )
  )
  res <- cli$post(path = path, body = body, encode = encode)
  if (res$status_code != 200) {
    code_number <- res$status_code
    request_url <- res$request$url
    abort(glue("Status code {code_number} returned for url {request_url}."),
                call = error_call)
  }
  res$parse("UTF-8")
}
