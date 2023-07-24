#' Internal function to run a GET call using httr2
#' @noRd
#' @keywords Internal
#' @importFrom httr2 request
#' @importFrom httr2 req_error
#' @importFrom httr2 req_headers
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json 
#' @importFrom glue glue
#' @importFrom rlang inform
url_GET <- function(url, 
                    params = list(), 
                    slot_name = NULL,
                    error_call = caller_env()) {
  
  url <- build_url_internal(list(url = url, query = params))
  response <- request(url) |>
    req_headers("User-Agent" = galah_version_string()) |>
    req_error(is_error = NULL) |> # untested; intended to catch errors
    req_perform()
  
  if(is.null(response)){
    NULL
  }else{
    resp_body_json(response)
  }
}


#' parse a returned object from url_GET or similar
#' @noRd
#' @keywords Internal
#' @importFrom jsonlite fromJSON
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
