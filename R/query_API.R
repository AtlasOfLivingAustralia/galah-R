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
query_API <- function(.query, error_call = caller_env()) {
  if(inherits(.query$url, "data.frame")){
    verbose <- pour("package", "verbose", .pkg = "galah") & nrow(.query$url) > 1
    if(verbose){
      progress_bar <- list(name = "Querying API",
                           clear = TRUE)
    }else{
      progress_bar <- FALSE
    }
    map(.x = seq_len(nrow(.query$url)), 
        .f = function(a){
          data_tr <- .query
          data_tr$url <- .query$url$url[[a]]
          if(any(names(.query$url) == "path")){ # for those that require downloads
            data_tr$download <- TRUE
            data_tr$file <- .query$url$path[[a]]
          }
          query_API_internal(data_tr)
        },
        .progress = progress_bar)
  }else{
    query_API_internal(.query)
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
#' @importFrom httr2 req_timeout
#' @importFrom httr2 resp_body_json
#' @importFrom httr2 resp_body_string
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @importFrom rlang inform
query_API_internal <- function(.query, error_call = caller_env()) {
  query <- request(.query$url) |>
    add_headers(.query$headers) |> 
    add_options(.query$options) |> # used by GBIF
    add_body(.query$body)  # NOTE: adding `body` converts from GET to POST
  # handle downloads first
  if(!is.null(.query$download)){
    check_directory(.query$file)
    
    # handle thumbnails (which might fail if missing)
    if (any(str_detect(.query$url, "thumbnail"))) {
      query |> 
        req_error(is_error = \(resp) FALSE) |>
        req_perform(path = .query$file,
                    verbosity = 0)
    } else {
      query |> 
        req_perform(path = .query$file,
                    verbosity = 0)
    }
  # then other pings, which should resolve quickly 
  # and can be allowed to fail otherwise
  }else{
    res <- query |>
      req_timeout(seconds = 20) |>
      req_perform(verbosity = 0)
    if(grepl("^https://api.gbif.org/v1/occurrence/download/request", .query$url)){
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
#' @importFrom httr2 req_body_raw
add_body <- function(req, body){
  if(!is.null(body)){
    req <- req |> req_body_raw(body)
    # note: this is not `req_body_json()` because 
    # we have already converted our list to json text
    # by this point
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
