#' Set up authentication
#' 
#' Authenticate a request, either by sending a registered email address
#' (and, for GBIF, password and username); or by loggin in via the 
#' browser to generate a JWT token. Note that while handling this manually 
#' in-pipe is the most transparent approach, this function is also used to 
#' pass cached information stored via [galah_config()] to a query.
#' `r lifecycle::badge("experimental")`.
#' @param .data An object of class `data_request` or `metadata_request`.
#' @param email (string) Email address registered with the selected organisation.
#' @param username (string) Registered username (GBIF only).
#' @param password (string) Registered password (GBIF only).
#' @param download_reason_id (integer) ID for the download reason. 
#' See `show_all_reasons()` for accepted values.
#' @param use_jwt (logical) Should an OAuth workflow be used for authentication?
#' Only supported for Flemish and Australian atlases. Defaults to `FALSE`
#' @param cache_jwt (logical) Should JWT tokens be cached to disk? Defaults
#' to `FALSE`.
#' @param ... Other arguments, currently ignored.
#' @returns An object of the same class as supplied, but with an added
#' `authenticate` slot.
#' @examples \dontrun{
#' # use `galah_config()` to set for all occurrence queries
#' galah_config(authenticate = TRUE)
#' 
#' x <- galah_call() |>
#'   identify("Wollemia nobilis") |>
#'   collect()
#' 
#' # use in-pipe for more control
#' x <- galah_call() |>
#'   identify("Wollemia nobilis") |>
#'   authenticate() |>
#'   collect()
#' }
#' @export
authenticate <- function(.data,
                         email = NULL,
                         username = NULL,
                         password = NULL,
                         download_reason_id = NULL,
                         use_jwt = FALSE,
                         cache_jwt = FALSE,
                         ...){

  # handle GBIF first
  if(.data$atlas == "Global"){
    # only occurrence queries require authentication for this atlas
    if(authentication_required(.data)){
      # ignore token requests
      if(isTRUE(use_jwt)){
        cli::cli_warn("JWT-based authentication not supported for GBIF: skipping")
      }
      authenticate_with_email_gbif(.data,
                                   email = email,
                                   download_reason_id = download_reason_id,
                                   username = username,
                                   password = password)
    }else{
      .data
    }
  # Now handle living atlases
  # behaviour here differs from gbif - support authentication wherever possible
  # specifically, call JWT if authentication is possible, and either
    # 1. user has specifically requested it, or 
    # 2. no email is provided
  }else{
    # get the download reason, if not supplied
    if(is.null(download_reason_id)){
      download_reason_id <- potions::pour("user", "download_reason_id")
    }
    # NOTE: We don't _check_ the reason here, because that has to happen 
    # at `collapse()`, because we need to ping an API first
    
    if(
      (isTRUE(use_jwt) | is.null(email)) & authentication_supported(.data$atlas)
    ){
      update_request_object(.data, 
                            authenticate = list(email = NULL,
                                                download_reason_id = download_reason_id,
                                                use_jwt = TRUE,
                                                use_apikey = FALSE, # not supported yet
                                                cache_disk = cache_jwt))
    }else{
      authenticate_with_email(.data,
                              email = email,
                              download_reason_id = download_reason_id)
    }
  }
}

#' Internal function to build a valid email authentication for GBIF
#' @noRd
#' @keywords Internal
authenticate_with_email <- function(.data,
                                     email = NULL, 
                                     download_reason_id,
                                     call = rlang::caller_env()){
  if(is.null(email)){
    email <- potions::pour("user", "email", .pkg = "galah")
  }
  if(any(is.null(email) | nchar(email) < 1 | is.na(email))){
    abort_email_missing(call = call)
  }
  update_request_object(.data,
                        authenticate = list(email = email,
                                            download_reason_id = download_reason_id,
                                            use_jwt = FALSE,
                                            use_apikey = FALSE,
                                            cache_disk = FALSE))
}

#' Internal function to build a valid email authentication for GBIF
#' @noRd
#' @keywords Internal
authenticate_with_email_gbif <- function(.data,
                                         email = NULL,
                                         username = NULL,
                                         password = NULL,
                                         download_reason_id,
                                         call = rlang::caller_env()){
  # look for local email etc
  email_args <- c(email, username, password)
  if(any(is.null(email_args))){
    cached_user <- potions::pour("user", .pkg = "galah")
    email_args <- unlist(cached_user)[c("email", "username", "password")]
  }
  # run some checks
  email_args <- purrr::map(email_args, enforce_character) |>
    unlist()
  if(any(is.null(email_args) | nchar(email_args) < 1 | is.na(email_args), na.rm = TRUE)){
    c("GBIF requires the following information for occurrence queries:",
      i = "`email`, `username`, `password` and `download_reason_id`",
      i = "These can be supplied globally via {.fn galah::galah_config}, or in-pipe via {.fn galah::authenticate}.") |>
    cli::cli_abort(call = call)
  }
  # save out
  update_request_object(.data,
                        authenticate = list(email = email,
                                            username = username,
                                            password = password,
                                            download_reason_id = download_reason_id,
                                            use_jwt = FALSE,
                                            use_apikey = FALSE,
                                            cache_disk = FALSE))
}