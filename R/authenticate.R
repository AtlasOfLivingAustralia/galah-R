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
        "This function will open an authentication screen in your browser.",
        " "),
      "Do you want to continue? (0 to exit)",
      choices = c("Yes", "No")
    )
    
    if (choice == 1) {
      # authenticate now
      auth_info <- get_auth_info()
      result <- httr2::oauth_flow_auth_code(
        client = auth_info$client,
        auth_url = dplyr::pull(auth_info$config, "authorize_url"),
        scope = dplyr::pull(auth_info$config, "scopes"),
        pkce = TRUE)
      galah_config(authenticate = TRUE) # cache authentication behaviour
      return(invisible(result)) # invisibly return token
      
    } else {
      cli::cli_bullets(c(
        i = "Exiting..."
      ))
      # exits process quietly
      invokeRestart("abort")
    }
    invisible()
    
  } 
}

#' @rdname authenticate
#' @export
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
    isTRUE(potions::pour("package",
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

#' Interactive menu function
#' @description
#' Built on top of utils::menu(). 
#' Originally proposed by Hadley here: https://github.com/r-lib/cli/issues/228#issuecomment-1453614104
#' Full code from gargle here: https://github.com/r-lib/gargle/blob/main/R/utils-ui.R
#' This version updated from `galaxias` v. 0.1.0
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
  
  cli::cli({
    cli::cli_text(header, .envir = .envir)
    cli::cli_text("", .envir = .envir)
    cli::cli_text(prompt, .envir = .envir)
    cli::cli_text("", .envir = .envir)
    cli::cli_bullets(choices, .envir = .envir)
  })
  
  repeat {
    selected <- cli_readline("Selection: ")
    if (selected %in% c("0", seq_along(choices))) {
      break
    }
    cli::cli_text(
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

#' Interactive readLines
#' @description
#' Allows for interactive testing of `cli_menu()` selection. 
#' Originally proposed by Hadley here: https://github.com/r-lib/cli/issues/228#issuecomment-1453614104.
#' Full code from gargle here: https://github.com/r-lib/gargle/blob/main/R/utils-ui.R
#' @noRd
#' @keywords Internal
cli_readline <- function(prompt) {
  local_input <- getOption("cli_input", character())
  
  # not convinced that we need to plan for multiple mocked inputs, but leaving
  # this feature in for now
  if (length(local_input) > 0) {
    input <- local_input[[1]]
    cli::cli_text(paste0(prompt, input))
    options(cli_input = local_input[-1])
    input
  } else {
    readline(prompt)
  }
}

## -- testing -- ##

#' Mimic supplying user input to a menu
#' @noRd
#' @keywords Internal
local_user_input <- function(x, env = rlang::caller_env()) {
  withr::local_options(
    rlang_interactive = TRUE,
    # trailing 0 prevents infinite loop if x only contains invalid choices
    cli_input = c(x, "0"),
    .local_envir = env
  )
}

#' Check whether function is being called by testthat
#' @noRd
#' @keywords Internal
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
