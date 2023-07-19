#' Internal function to run a GET call using httr2
#' @noRd
#' @keywords Internal
#' @importFrom httr2 request
#' @importFrom httr2 req_error
#' @importFrom httr2 req_headers
#' @importFrom httr2 req_perform
#' @importFrom rlang abort
#' @importFrom rlang inform
query_API <- function(.data,
                      # url, 
                      # params = list(), # Obsolete: should be built during `collapse()`?
                      # slot_name = NULL, # relates to parsing out content
                      error_call = caller_env()) {
  
  # NOTE: how to handle multiple urls? Is this automatic?
  
  # convert to check_api_key()
  check_api_key(.data)
  
  # build and run API call
  # url <- url_build_internal(list(url = url, query = params)) # enforce parsing beforehand: obsolete
  result <- request(.data$url) |>
    add_headers(.data$headers) |> 
    add_options(.data$options) |> # used by GBIF
    add_body(.data$body) |> # NOTE: adding `body` converts from GET to POST
    req_error(is_error = ~ FALSE) |> # untested; intended to catch errors. 
    # from brief testing it appears to fail; e.g. we still get errors when internet is off
    req_perform(path = .data$path) |> # if path != NULL, caches as per url_download
    resp_body_json() # may not work for invalid URLs
  
  # subset to particular slot if needed  
  if(!is.null(.data$slot_name)){
    result <- result[[.data$slot_name]]
  }
  
  if(inherits(result, "list")){
    lapply(result, function(a){a[lengths(a) == 1]}) |>
    bind_rows()
  }else{
    result
  }
  # # return correct info.
  # # NOTE: is this needed? Test with stuff that returns errors
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
#' @importFrom potions pour
add_headers <- function(req, headers){
  if(!is.null(headers)){
    req$headers <- headers
    req
  }else{
    if(pour("atlas", "acronym") == "ALA"){
      req |> req_headers(
        "User-Agent" = galah_version_string(),
        "x-api-key" = pour("user", "api_key"))
    }else{
      req |> req_headers("User-Agent" = galah_version_string())
    }
  }
}

#' If supplied, add `body` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom httr2 req_body_json
add_body <- function(req, body){
  if(!is.null(body)){
    req$body <- body
  }
  req
}

#' If supplied, add `options` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom httr2 req_options
add_options <- function(req, options){
  if(!is.null(options)){
    req$options <- options
  }
  req
}