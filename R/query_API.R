#' Internal function to call APIs
#' 
#' Note that this is a wrapper to `query_API_internal()` to handle single or 
#' multiple urls. Multiple urls *must* be given as a tibble, which *must* have a 
#' column named `url`.
#' @noRd
#' @keywords Internal
query_API <- function(.query, 
                      error_call = rlang::caller_env()) {
  # first try situation when many urls are supplied
  # this is common for living atlases, where many urls are generated for
  # e.g. paginated queries, grouped counts etc
  if(inherits(.query$url, "data.frame")){
    purrr::map(.x = seq_len(nrow(.query$url)), 
        .f = function(a){
          data_tr <- .query
          data_tr$url <- .query$url$url[[a]]
          if(any(names(.query$url) == "path")){ # for those that require downloads
            data_tr$download <- TRUE
            data_tr$file <- .query$url$path[[a]]
          }
          query_API_internal(data_tr,
                             error_call = error_call)
        },
        .progress = set_progress_bar_behaviour(nrow(.query$url) > 1))
  # next handle multiple `body` arguments
  # this is currently limited to GBIF count requests with > 1 `group_by` args
  }else if(inherits(.query$body, "data.frame")){ 
    purrr::map(.x = split(.query$body, 
                          seq_len(nrow(.query$body))),
               .f = function(a){
                 data_tr <- .query
                 data_tr$body <- a$predicate[[1]]
                 a$result <- list(query_API_internal(data_tr,
                                                     error_call = error_call))
                 a
               },
               .progress = set_progress_bar_behaviour(length(.query$body) > 1)) |>
      dplyr::bind_rows()
  # finally, some queries are 'simple'; one `url`, one or no `body` args
  # these we just run without any looping.
  }else{
    query_API_internal(.query,
                       error_call = error_call)
  }
}

#' Internal function to run an API call using httr2
#' @param criteria length-1 logical statement as to whether to proceed or not
#' @noRd
#' @keywords Internal
set_progress_bar_behaviour <- function(criteria){
  verbose <- potions::pour("package", "verbose", .pkg = "galah") &
    criteria
  if(verbose){
    progress_bar <- list(name = "Querying API",
                         clear = TRUE)
  }else{
    progress_bar <- FALSE
  }
}

#' Internal function to run an API call using httr2
#' @noRd
#' @keywords Internal
query_API_internal <- function(.query,
                               error_call = rlang::caller_env()) {
  query <- httr2::request(.query$url) |>
    add_headers(.query$headers) |> 
    add_options(.query$options) |> # used by GBIF
    add_body(.query$body)  # NOTE: adding `body` converts from GET to POST

  # set authentication behaviour
  if(potions::pour("package", "authenticate", .pkg = "galah") &
     .query$type != "metadata/config" # necessary to prevent circular problems
     ){
    # check whether config data is available
    auth_config <- retrieve_cache("config")
    if(is.null(auth_config)){
      cli::cli_abort(c("`authenticate` is set to `TRUE`, but `config` data is not available",
                       i = "Call `request_metadata(type = \"config\") |> collect()`, then try again"),
                     call = error_call)
    }else{
      query <- query |>
        httr2::req_oauth_auth_code(client = build_auth_client(auth_config),
                                   auth_url = dplyr::pull(auth_config, "authorize_url"),
                                   scope = dplyr::pull(auth_config, "scopes"),
                                   pkce = TRUE,
                                   cache_disk = TRUE)
    }
  }

  # then handle downloads
  if(!is.null(.query$download)){
    check_directory(.query$file)
    
    # handle thumbnails (which might fail if missing)
    if(any(stringr::str_detect(.query$url, "thumbnail"))) {
      query |> 
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform(path = .query$file,
                           verbosity = 0)
    } else {
      query |> 
        httr2::req_perform(path = .query$file,
                           verbosity = 0)
    }
  # then other pings, which should resolve quickly 
  # and can be allowed to fail otherwise
  }else{
    res <- query |>
      httr2::req_timeout(seconds = 20) |>
      httr2::req_perform(verbosity = 0)
    if(grepl("^https://api.gbif.org/v1/occurrence/download/request", .query$url)){
      httr2::resp_body_string(res)
    }else{
      httr2::resp_body_json(res) # may not work for invalid URLs 
    }
  }
}

#' create a client object
#' @noRd
#' @keywords Internal
build_auth_client <- function(config){
  httr2::oauth_client(
    id = dplyr::pull(config, "client_id"),
    token_url = dplyr::pull(config, "token_url"))  
}

#' If supplied, add `headers` arg to a `request()`
#' @noRd
#' @keywords Internal
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
add_body <- function(req, body){
  if(!is.null(body)){
    req <- req |> httr2::req_body_raw(body)
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
