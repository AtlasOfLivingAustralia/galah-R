#' Internal function to call APIs
#' 
#' Note that this is a wrapper to `query_API_internal()` to handle single or 
#' multiple urls. Multiple urls *must* be given as a tibble, which *must* have a 
#' column named `url`.
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom purrr map
#' @noRd
#' @keywords Internal
query_API <- function(.data, error_call = caller_env()) {
  if(inherits(.data$url, "data.frame")){
    verbose <- pour("package", "verbose", .pkg = "galah") & nrow(.data$url) > 1
    if(verbose){
      progress_bar <- list(name = "Querying API",
                           clear = TRUE)
    }else{
      progress_bar <- FALSE
    }
    map(.x = seq_len(nrow(.data$url)), 
        .f = function(a){
          data_tr <- .data
          data_tr$url <- .data$url$url[[a]]
          if(any(names(.data$url) == "path")){ # for those that require downloads
            data_tr$download <- TRUE
            data_tr$file <- .data$url$path[[a]]
          }
          query_API_internal(data_tr)
        },
        .progress = progress_bar)
  }else{
    query_API_internal(.data)
  }
}

#' Internal function to run an API call using httr2
#' @noRd
#' @keywords Internal
#' @importFrom dplyr bind_rows
#' @importFrom httr2 request
#' @importFrom httr2 req_error
#' @importFrom httr2 req_headers
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_string
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @importFrom rlang inform
query_API_internal <- function(.data, error_call = caller_env()) {
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
    res <- query |>
      req_perform(verbosity = 0)  # try(x, silent = TRUE) ?
    if(grepl("^https://api.gbif.org/v1/occurrence/download/request", .data$url)){
      resp_body_string(res)
    }else{
      resp_body_json(res) # may not work for invalid URLs 
    }
  }
}

#' If supplied, add `headers` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
add_headers <- function(req, headers){
  if(!is.null(headers)){
    req$headers <- headers
  }else{
    req$headers <- build_headers()
  }
  req
}

#' If supplied, add `body` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom httr2 req_body_json
add_body <- function(req, body){
  if(!is.null(body)){
    req <- req |>
      req_body_json(body)
    req$body$params <- NULL
  }
  req
}

#' If supplied, add `options` arg to a `request()`
#' @noRd
#' @keywords Internal
add_options <- function(req, options){
  if(!is.null(options)){
    req$options <- options
  }
  req
}