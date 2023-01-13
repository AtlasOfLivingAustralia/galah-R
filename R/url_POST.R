# Wrapper for getting data
# 
# NOTE: at ALA, this is only used for caching long queries:
# atlas_ functions > build_query > cached_query > url_POST
# this functionality is currently disabled

# For GBIF, it is used by the download service


url_POST <- function(url, 
                     body,
                     opts,
                     headers,
                     error_call = caller_env()
                     ) {

  cli <- crul::HttpClient$new(
    url = url,
    opts = opts,
    headers = headers)

  res <- try(cli$post(body = body), silent = TRUE)

  if(inherits(res, "try-error")){
    return(NULL)
  }else{
    if (res$status_code != 201) {
        code_number <- res$status_code
        request_url <- res$request$url
        inform(glue("Status code {code_number} returned for url {request_url}."),
                call = error_call)
        return(NULL)
      }else{
        res$parse("UTF-8")
      }
  }
}
# example output: "0229238-220831081235567"