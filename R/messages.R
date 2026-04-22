#' Internal function called by `authenticate()`
#' @noRd
#' @keywords Internal
abort_email_missing <- function(error_call = rlang::caller_env()){
  c(
    "No user email was found.",
    i = "To download occurrence records, species lists, or (for GBIF) occurrence
      counts, \\
      you must provide a valid email address registered with the selected atlas.",
    i = "Provide your email address using `galah_config(email = )` or `authenticate(email = )`.") |>
  cli::cli_abort(call = error_call)
}
# generalise this to discuss API keys, use of `authenticate()` as in-pipe alternative
# to `galah_config()`

#' System-wide, generic failure message
#' @noRd
#' @keywords Internal
system_down_message <- function(function_name, 
                                error_call = rlang::caller_env()){
  c(
    "Calling the API failed for `{function_name}`.",
    i = "This might mean that the API is down, or that you are not connected to the internet.",
    i = "Double check that your query is correct, or try again later."
  ) |>
  cli::cli_abort(call = error_call)
}