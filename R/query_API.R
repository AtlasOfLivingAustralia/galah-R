#' Internal function to call APIs
#' Note that this is a wrapper to `query_API_internal()` to handle single or multiple urls
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
query_API <- function(.data, error_call = caller_env()) {
  if(length(.data$url) > 1 | inherits(.data$url, "list")){
    lapply(.data$url,
           function(a){
             data_tr <- .data
             data_tr$url <- a
             query_API_internal(data_tr)
           }) |>
      bind_rows()
  }else{
    query_API_internal(.data)
  }
}

#' Internal function to run a GET call using httr2
#' @noRd
#' @keywords Internal
#' @importFrom dplyr bind_rows
#' @importFrom httr2 request
#' @importFrom httr2 req_error
#' @importFrom httr2 req_headers
#' @importFrom httr2 req_perform
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @importFrom rlang inform
query_API_internal <- function(.data, error_call = caller_env()) {
  # construct and run query
  query <- request(.data$url) |>
    add_headers(.data$headers) |> 
    add_options(.data$options) |> # used by GBIF
    add_body(.data$body) |> # NOTE: adding `body` converts from GET to POST
    req_error(is_error = ~ FALSE) # untested; intended to catch errors. 
    # from brief testing it appears to fail; e.g. we still get errors when internet is off
  
  if(!is.null(.data$download)){
    query |> req_perform(path = .data$file,
                         verbosity = 0) # try(x, silent = TRUE) ?
  }else{
    result <- query |>
      req_perform(verbosity = 0) |>  # try(x, silent = TRUE) ?
      resp_body_json() # may not work for invalid URLs
    # subset to particular slot if needed  
    if(!is.null(.data$slot_name)){
      result <- pluck(result, !!!.data$slot_name)
    }
    # clean up and return
    clean_json(result, .data$return_basic)
  }
}


#' Internal function to clean up objects returned by the API
#' @noRd
#' @keywords Internal
clean_json <- function(result, return_basic = NULL){
  # rbind if not requested otherwise
  if(is.null(return_basic) && inherits(result, "list")){
    if(most_common_integer(lengths(result)) > 1){
      # e.g. collect_lists(), where there are many lists, each containing a tibble 
      lapply(result, function(a){a[lengths(a) == 1]}) |>
        bind_rows()
    }else{
      # e.g. collect_taxa(), where the whole list is a single tibble
      keep <- lapply(result,
                     function(a){lengths(a) == 1 & !inherits(a, "list")}) |>
        unlist()
      bind_rows(result[keep])
    }
  }else{
    result
  }
}

#' simple function to show most frequent value; used for assessing list size
#' @noRd
#' @keywords Internal
most_common_integer <- function(x){
  result <-sort(xtabs(~x), decreasing = TRUE)[1]
  as.integer(names(result)[1])
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
    if(pour("atlas", "acronym", .pkg = "galah") == "ALA"){
      req |> req_headers(
        "User-Agent" = galah_version_string(),
        "x-api-key" = pour("user", "api_key", .pkg = "galah"))
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