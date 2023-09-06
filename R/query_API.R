#' Internal function to call APIs
#' 
#' Note that this is a wrapper to `query_API_internal()` to handle single or 
#' multiple urls. Multiple urls *must* be given as a tibble, which *must* have a 
#' column named `url`.
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr slice
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @noRd
#' @keywords Internal
query_API <- function(.data, error_call = caller_env()) {
  if(inherits(.data$url, "data.frame")){
    verbose <- pour("package", "verbose", .pkg = "galah") & nrow(.data$url) > 1 
    if (verbose) { 
      pb <- txtProgressBar(max = 1, style = 3) 
    }else{ 
      pb <- NULL
    }
    n <- seq_len(nrow(.data$url))
    lapply(n, function(a){
      data_tr <- .data
      data_tr$url <- .data$url$url[[a]]
      # for those that require downloads:
      if(any(names(.data$url) == "path")){
        data_tr$path <- .data$url$path[[a]]
      }
      result <- query_API_internal(data_tr)
      if (verbose) {
        val <- (a / max(n))
        setTxtProgressBar(pb, val)
      }
      return(result)
    })
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
#' @importFrom httr2 resp_body_json
#' @importFrom purrr pluck
#' @importFrom rlang abort
#' @importFrom rlang inform
query_API_internal <- function(.data, error_call = caller_env()) {
  
  ## note: when using jwt may need to do something like:
    # tokens <- api_authenticate()
    # .data$header <- list("x-api-key" = tokens$apikey, 
    #                      "Authorization" = paste("Bearer", token$access_token)
  ## The obvious problem is that this slows everything down, and is extremely 
  ## circular (i.e. query_API() would presumably have to call itself?!), so 
  ## would need to look carefully at caching behaviour
  
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
    query |>
      req_perform(verbosity = 0) |>  # try(x, silent = TRUE) ?
      resp_body_json() # may not work for invalid URLs
  }
}

# # if first level is a list of length 1, which contains the actual data, subset
# if(is.list(result) & length(result[[1]]) > 0){
#   if(length(result) == 1){
#     result <- result[[1]]  
#     # subset to particular slot if needed  
#     if(!is.null(.data$slot_name)){
#       result <- pluck(result, !!!.data$slot_name)
#     }
#   }else{
#     if(!is.null(.data$slot_name)){
#       result <- lapply(result, function(a){pluck(a, !!!.data$slot_name)})
#     }
#   }
# }

#' Internal function to clean up objects returned by the API
#' @noRd
#' @keywords Internal
# clean_json <- function(result, return_basic = NULL){
#   # rbind if not requested otherwise
#   if(is.null(return_basic) && inherits(result, "list")){
#     if(most_common_integer(lengths(result)) > 1){
#       # e.g. collect_lists(), where there are many lists, each containing a tibble 
#       lapply(result, function(a){a[lengths(a) == 1]}) |>
#         bind_rows()
#     }else{
#       # e.g. collect_taxa(), where the whole list is a single tibble
#       keep <- lapply(result,
#                      function(a){lengths(a) == 1 & !inherits(a, "list")}) |>
#         unlist()
#       bind_rows(result[keep])
#     }
#   }else{
#     result
#   }
# }

#' simple function to show most frequent value; used for assessing list size
#' @noRd
#' @keywords Internal
# most_common_integer <- function(x){
#   result <-sort(xtabs(~x), decreasing = TRUE)[1]
#   as.integer(names(result)[1])
# }

#' If supplied, add `headers` arg to a `request()`
#' @noRd
#' @keywords Internal
#' @importFrom httr2 req_headers
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