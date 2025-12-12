#' Set up authentication
#' 
#' This is early-stage code. It is triggered from 
#' `galah_config(authenticate = TRUE)`, but given the package-wide importance
#' of authentication it seemed wise to collect all functions in one place. 
#' @noRd
#' @keywords Internal
use_authentication <- function(.data,
                               cache_disk = FALSE){
  .data$authenticate <- list(
    use_jwt = TRUE,
    use_apikey = FALSE, # not supported yet
    cache_disk = cache_disk)
  .data
}

#' Internal function to lookup requests for authentication
#' Note this is currently only called on `data_request` objects, and 
#' then only before parsing
#' @noRd
#' @keywords Internal
check_authentication <- function(x){
  if(
    isTRUE(potions::pour("user",
                         "authenticate",
                         .pkg = "galah")) &
    x$type %in% c("occurrences") # possible to add other allowed queries
  ){
    x |> use_authentication()
  }else{
    x
  }
}

#' Internal function to pass authentication information forward 
#' @noRd
#' @keywords Internal
retain_authentication <- function(source, x){
  if(
    !is.null(source$authenticate) & # i.e.. authenticate was supplied
     is.null(x$authenticate)        # but was then lost
    ){
    x$authenticate <- source$authenticate
  }
  x
} 

#' get a client, and if it doesn't exist, make one
#' @noRd
#' @keywords Internal
get_auth_info <- function(error_call = rlang::caller_env()){
  x <- retrieve_cache("client") # this is cached by build_auth_client()
  auth_config <- show_all_config() # handle download /retrieval of config info
  if(is.null(x)){
    x <- build_auth_client(auth_config)
  }
  # if still can't get a client, you might be offline
  if(is.null(x)){
    cli::cli_abort(c("Unable to generate an authentication client",
                     i = "You might be offline"),
                   call = error_call)
  }
  list(config = auth_config,
       client = x)
}

#' create a client object
#' @noRd
#' @keywords Internal
build_auth_client <- function(config){
  result <- httr2::oauth_client(
    id = dplyr::pull(config, "client_id"),
    token_url = dplyr::pull(config, "token_url"),
    auth = "body",
    name = "galah")
  update_cache(client = result)
  result
}