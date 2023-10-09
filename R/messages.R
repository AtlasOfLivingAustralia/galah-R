#' Internal function called by `check_login()`
#' @noRd
#' @keywords Internal
abort_email_missing <- function(error_call = caller_env()){
  bullets <- c(
    "No user email was found.",
    i = glue(
      "To download occurrence records or species lists you must provide a valid email \\
      address registered with the selected atlas."
    ),
    i = glue("Provide your email address using `galah_config(email = )`.")
  )
  abort(bullets, call = error_call)
}

# Internal function called by `check_login()`
# @noRd
# @keywords Internal
# abort_api_key_missing <- function(error_call = caller_env()){
#   bullets <- c("API key has not been specified for the ALA",
#                i = "log on to your profile at `https://ala.org.au` to retrieve one",
#                i = "use `galah_config(api_key = 'my_key_here') to fix this problem"
#   )
#   abort(bullets, call = error_call)
# }

#' System-wide, generic failure message
#' @noRd
#' @keywords Internal
system_down_message <- function(function_name){
  bullets <- c(
    glue("Calling the API failed for `{function_name}`."),
    i = "This might mean that the API is down, or that you are not connected to the internet.",
    i = "Double check that your query is correct, or try again later."
  )
  inform(bullets)
}