#' @rdname galah_call
#' @param x an object of class `data_request`
#' @export
print.data_request <- function(x, ...){
  filled_slots <- !unlist(lapply(x, is.null))
  if(any(filled_slots)){
    cat("An object of type `data_request` containing:\n\n")
    print(as.list(x[filled_slots]))
  }else{
    cat("An empty object of type `data_request`")
  }
}
# NOTE: use of `x` arg here is for consistency with `print()`; do not change

#' @rdname galah_config
#' @param x an object of class `galah_config`
#' @importFrom rlang format_error_bullets
#' @importFrom crayon bold
#' @export
print.galah_config <- function(x, ...){
  package_settings <- c("verbose", "run_checks", "send_email")
  package_lookup <- unlist(x$package[1:3]) |> as.integer() + 1
  names(package_settings) <- c("x", "v")[package_lookup]
  package_settings <- c(package_settings,
                        "i" = glue("directory: {x$package$directory}"))
                        
  values <- c(
    "username: {x$user$username}",
    "email:    {x$user$email}",
    "password: {x$user$password}",
    "api_key:  {x$user$api_key}",
    "reason:   {x$user$download_reason_id}")
  password_settings <- lapply(values, 
                              function(a, x){glue_data(x, a)}, x = x) |>
    unlist()

  atlas_settings <- glue(" {x$atlas$organisation} ({x$atlas$acronym}) [{x$atlas$region}]")
  names(atlas_settings) <- ">"

  print_text <- c(
    crayon::bold("Package:"),
    package_settings,
    "",
    crayon::bold("User:"),
    password_settings,
    "",
    crayon::bold("Atlas:"),
    atlas_settings
    )

  cat(format_error_bullets(print_text))
}

#' @rdname compute.data_request
#' @param x an object of class `data_response`
#' @export
print.data_response <- function(x, ...){
  str(x)
}

#' Internal function to prevent showing secret information in the console
#' @noRd
#' @keywords Internal
hide_secrets <- function(string){
  if(string == ""){
    "[Not Provided]"
  }else{
    "[Provided]"
  }
}