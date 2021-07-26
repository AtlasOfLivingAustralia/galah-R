# Wrapper for getting data
# 
# Try using crul
ala_POST <- function(url, path, body = list(), encode = "form") {
  cli <- HttpClient$new(
    url = url,
    headers = list(
      "User-Agent" = galah_version_string()
    )
  )
  res <- cli$post(path = path, body = body, encode = encode)
  if (res$status_code != 200) {
    stop("Status code ", res$status_code, "returned for url ", res$request$url)
  }
  res$parse("UTF-8")
}