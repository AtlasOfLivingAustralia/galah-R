#' Internal function to run a GET call using httr2
#' @noRd
#' @keywords Internal
#' @importFrom httr2 request
#' @importFrom httr2 req_error
#' @importFrom httr2 req_headers
#' @importFrom httr2 req_perform
#' @importFrom glue glue
#' @importFrom rlang inform
query_API <- function(url, 
                      # params = list(), # q: should this be constructed here? Or by `collapse()`?
                      headers = NULL,
                      options = NULL, # used by GBIF
                      body = NULL,
                      path = NULL,
                      # slot_name = NULL, # relates to parsing out content
                      error_call = caller_env()) {
  
  # build and run API call
  # url <- url_build_internal(list(url = url, query = params)) # enforce parsing beforehand
  request(url) |>
    add_headers(headers) |> 
    add_options(options) |>
    add_body(body) |> # NOTE: adding `body` converts from GET to POST
    req_error(is_error = ~ FALSE) |> # untested; intended to catch errors
    req_perform(path = path) |> # if path != NULL, caches as per url_download
    resp_body_json() # may not work for invalid URLs
  
  # # return correct info. NOTE: is this needed? Test with stuff that returns errors
  # # If not needed, convert whole function to a single pipe
  # if(is.null(response)){
  #   NULL
  # }else{
  #   resp_body_json(response)
  # }
}

#' If supplied, add `headers` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom httr2 req_headers
add_headers <- function(req, headers){
  if(!is.null(headers)){
    args_list <- c(list(".req" = req), headers)
    do.call(args_list, req_headers)
  }else{
    req |> req_headers("User-Agent" = galah_version_string())
  }
}

#' If supplied, add `body` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom httr2 req_body_json
add_body <- function(req, body){
  if(!is.null(body)){
    req |> req_body_json(body)
  }else{
    req
  }
}

#' If supplied, add `options` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom httr2 req_options
add_options <- function(req, options){
  if(!is.null(options)){
    req |> req_options(options)
  }else{
    req
  }
}