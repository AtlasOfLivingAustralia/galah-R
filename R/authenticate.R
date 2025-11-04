#' Set up authentication
#' 
#' This is early-stage code. It's purpose is to trigger the browser to generate 
#' a JWT token. [authenticate()] sets `galah_config(authenticate = TRUE)`, but 
#' differs in triggering authentication on execution (which `galah_config()` 
#' does not). [use_authentication()] is an in-pipe method for setting 
#' authentication and associated behaviour. It is primarily intended for 
#' internal use, but is exported for completeness and debugging purposes.
#' @name authenticate
#' @details
#' By default, authentication is only triggered during occurrence downloads
#' and query uploads. This can be overrided by calling [use_authentication()].
#' @export
authenticate <- function(){
  
  # offer user menu to confirm if not in batch run (testthat or knitr)
  if(rlang::is_interactive()){ 
    
    choice <- cli_menu(
      c(" ",
        "This function will open an authentication screen in your browser",
        " "),
      "Do you want to continue? (0 to exit)",
      choices = c("Yes", "No")
    )
    
    if (choice == 1) {
      # authenticate now
      # NOTE: I'm trialling the use of `oauth_flow_auth_code()` here.
      # This *should* generate a request to the browser without requiring a
      # dummy API call; but hasn't been found to work yet.
      auth_config <- show_all_config() # cache config info
      galah_config(authenticate = TRUE) # cache authentication behaviour
      result <- httr2::oauth_flow_auth_code(
        client = build_auth_client(auth_config),
        auth_url = dplyr::pull(auth_config, "authorize_url"),
        scope = dplyr::pull(auth_config, "scopes"),
        pkce = TRUE)
    } else {
      cli::cli_inform(c(
        i = "Exiting..."
      ))
      # exits process quietly
      invokeRestart("abort")
    }
    invisible()
    
  } 
}

#' @rdname authenticate
use_authentication <- function(.data,
                               cache_disk = FALSE){
  .data$authenticate <- list(
    use_jwt = TRUE,
    use_apikey = FALSE, # not supported yet
    cache_disk = cache_disk)
  .data
}


#' Interactive menu function
#' @description
#' Built on top of utils::menu(). 
#' Originally proposed by Hadley here: https://github.com/r-lib/cli/issues/228#issuecomment-1453614104
#' Full code from gargle here: https://github.com/r-lib/gargle/blob/main/R/utils-ui.R
#' This version borrowed verbatim from `galaxias` v. 0.1.0
#' @noRd
#' @keywords Internal
cli_menu <- function(header,
                     prompt,
                     choices,
                     not_interactive = choices,
                     exit = integer(),
                     .envir = rlang::caller_env(),
                     error_call = rlang::caller_env()) {
  if (!rlang::is_interactive()) {
    cli::cli_abort(
      c(header, not_interactive),
      .envir = .envir,
      call = error_call
    )
  }
  
  choices <- paste0(cli::style_bold(seq_along(choices)), ": ", choices)
  cli::cli_inform(
    c(header, prompt, choices),
    .envir = .envir
  )
  
  repeat {
    selected <- cli::cli_readline("Selection: ")
    if (selected %in% c("0", seq_along(choices))) {
      break
    }
    cli::cli_inform(
      "Enter a number between 1 and {length(choices)}, or enter 0 to exit."
    )
  }
  
  selected <- as.integer(selected)
  if (selected %in% c(0, exit)) {
    if (is_testing()) {
      cli::cli_abort("Exiting...", call = NULL)
    } else {
      cli::cli_alert_danger("Exiting...")
      # simulate user pressing Ctrl + C
      invokeRestart("abort")
    }
  }
  
  selected
}