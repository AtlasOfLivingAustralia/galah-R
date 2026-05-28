#' Internal function to call APIs
#' 
#' Note that this is a wrapper to `query_API_internal()` to handle single or 
#' multiple urls. Multiple urls *must* be given as a tibble, which *must* have a 
#' column named `url`.
#' @noRd
#' @keywords Internal
query_API <- function(.query, 
                      error_call = rlang::caller_env()) {
  # structure for executing queries
  if(inherits(.query$url, "data.frame")){
    query_API_multiple_url(.query, error_call = error_call)
  }else if(inherits(.query$body, "data.frame")){ 
    query_API_multiple_body(.query, error_call = error_call)
  }else{
    query_API_once(.query, error_call = error_call)
  }
}

#' Internal function to build an API call using httr2
#' @noRd
#' @keywords Internal
build_API_call <- function(.query,
                           error_call = rlang::caller_env()) {
  query <- httr2::request(.query$url) |>
    add_headers(.query$headers) |> 
    add_options(.query$options) |> # used by GBIF
    add_body(.query$body) |>  # NOTE: adding `body` converts from GET to POST
    httr2::req_retry(max_tries = 5, backoff = ~ 2)
    
  # set authentication behaviour
  if(isTRUE(.query$request$authenticate$use_jwt) & 
     .query$type != "metadata/config" # necessary to prevent circular problems
     ){

    # check whether config data is available
    auth_info <- get_auth_info(.query)
    query <- query |>
      httr2::req_oauth_auth_code(
        client = auth_info$client,
        auth_url = dplyr::pull(auth_info$config, "authorize_url"),
        scope = dplyr::pull(auth_info$config, "scopes"),
        pkce = TRUE,
        redirect_uri = "http://localhost:27231/",
        cache_disk = .query$request$authenticate$cache_disk)
  }

  return(query)
}

#' Internal function to run multiple url-based API calls using httr2
#' 
#' Built for situations when many urls are supplied.
#' This is common for living atlases, where many urls are generated for
#' e.g. paginated queries, grouped counts etc.
#' @noRd
#' @keywords Internal
query_API_multiple_url <- function(.query,
                                   error_call = rlang::caller_env()){
  # get a list of requests
  requests <- purrr::map(.x = seq_len(nrow(.query$url)), 
      .f = \(a){
        data_tr <- .query
        data_tr$url <- .query$url$url[[a]]
        build_API_call(data_tr)
      })
  # and, for downloads, paths
  paths <- purrr::map(.x = seq_len(nrow(.query$url)), 
      .f = \(a){
        if(any(names(.query$url) == "path")){ # for those that require downloads
          .query$url$path[[a]]
        }else{
          NA
        }
      }) |>
    unlist()

  # first look for paths; if present, download
  progress_object <- set_progress_bar_behaviour(nrow(.query$url) > 1)
  if(all(!is.na(paths))){
    # purrr::map(unique(paths), check_directory) # necessary?
    check_directory(paths[[1]]) # just check first path instead
    httr2::req_perform_parallel(requests,
                                paths = paths,
                                on_error = "continue",
                                progress = progress_object)
  }else{ # if no path, just run all queries
    result <- httr2::req_perform_parallel(requests,
                                          on_error = "continue",
                                          progress = progress_object)
    purrr::map(result, httr2::resp_body_json)
  }
}

#' Internal function to run multiple body-based API calls using httr2
#' 
#' This is currently limited to GBIF count requests with > 1 `group_by` args
#' @noRd
#' @keywords Internal
query_API_multiple_body <- function(.query,
                                    error_call = rlang::caller_env()){
  requests <- purrr::map(.x = split(.query$body, 
                                    seq_len(nrow(.query$body))),
               .f = \(a){
                 data_tr <- .query
                 data_tr$body <- a$predicate[[1]]
                 build_API_call(data_tr)
               })
  progress_object <- set_progress_bar_behaviour(nrow(.query$url) > 1)
  result <- httr2::req_perform_parallel(requests,
                                        on_error = "continue",
                                        progress = progress_object)
  purrr::map(result, httr2::resp_body_json)
}

#' Internal function to run an API call using httr2
#' @noRd
#' @keywords Internal
query_API_once <- function(.query,
                           error_call = rlang::caller_env()) {
  # build a query
  query <- build_API_call(.query)

  # handle downloads
  if(!is.null(.query$download)){
    check_directory(.query$file)
    query |> 
      httr2::req_perform(path = .query$file,
                         verbosity = 0)
  # then other pings, which should resolve quickly 
  # and can be allowed to fail otherwise
  }else{
    result <- query |>
      httr2::req_timeout(seconds = 20) |>
      httr2::req_perform(verbosity = 0)
    if(grepl("^https://api.gbif.org/v1/occurrence/download/request", .query$url)){
      httr2::resp_body_string(result)
    }else{
      httr2::resp_body_json(result) # may not work for invalid URLs 
    }
  }
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
    # event datasets have to be structured differently
    if(stringr::str_detect(req$url, "^https://api.ala.org.au/event")){
      req <- req |>
        httr2::req_body_json(data = list(query = body))
      # this looks weird because it stores a JSON-like string (actually a graphQL query)
      # within *another* JSON query. But it runs, so never mind.
    }else{
      req <- req |> 
        httr2::req_body_raw(body)
      # note: this is not `req_body_json()` because 
      # we have already converted our list to json text
      # by this point
    }
  }
  req
}

#' If supplied, add `options` arg to a `request()`
#' @noRd
#' @keywords Internal
add_options <- function(req, options){
  if(!is.null(options)){
    allowed <- c("httpauth", "userpwd")
    req$options <- options[names(options) %in% allowed]
  }
  req
}

#' get a client, and if it doesn't exist, make one
#' @noRd
#' @keywords Internal
get_auth_info <- function(.query,
                          error_call = rlang::caller_env()){
  # get config info
  auth_config <- request_metadata(type = "config",
                                  from = .query$atlas) |>
    collect()
  # use this to get a client
  auth_client <- build_auth_client(auth_config,
                                   atlas = .query$atlas)
  # note that both of the above functions check caches first, so it's not
  # inefficient to run both

  # if still can't get a client, you might be offline
  if(is.null(auth_client)){
    cli::cli_abort(c("Unable to generate an authentication client",
                     i = "You might be offline"),
                   call = error_call)
  }
  # otherwise, return the requested information
  list(config = auth_config,
       client = auth_client)
}

#' create a client object
#' @noRd
#' @keywords Internal
build_auth_client <- function(config, atlas){

  # check whether a client has been previously cached for this atlas
  cache_check <- retrieve_cache("client")
  if(!is.null(cache_check)){
    if(attr(cache_check, "region") == atlas){
      cache_check
    }
  }

  # security
  # check token url
  config |>
    dplyr::pull("token_url") |>
    authentication_host(atlas = atlas)

  # check auth url
  config |>
    dplyr::pull("authorize_url") |>
    authentication_host(atlas = atlas)

  # otherwise, build new client
  result <- httr2::oauth_client(
    id = dplyr::pull(config, "client_id"),
    token_url = dplyr::pull(config, "token_url"),
    auth = "body",
    name = "galah") |>
    update_attributes(type = "client",
                      atlas = atlas)
  
  # cache and return
  update_cache(client = result)
  result
}

#' Internal function to run an API call using httr2
#' @param criteria length-1 logical statement as to whether to proceed or not
#' @noRd
#' @keywords Internal
set_progress_bar_behaviour <- function(criteria){
  verbose <- all(
    potions::pour("package", "verbose", .pkg = "galah") &
    isTRUE(criteria))
  if(verbose){
    list(name = "Querying API",
         clear = TRUE)
  }else{
    FALSE
  }
}