# Wrapper for getting data
# 
# Try using crul

atlas_POST <- function(url, path, body = list(), encode = "form") {
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

internal_POST <- function(url, path, body, encode) {
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
    rlang_abort(glue("Status code {code_number} returned for url {request_url}."))
  }
  res$parse("UTF-8")
}
