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

  if(x$user$password == ""){
    password <- "[Not Provided]"
  }else{
    password <- "[Provided]"
  }
  
  package_settings <- c(
    paste0("verbose         username ", x$user$username),
    paste0("run_checks      password ", password),
    paste0("send_email      email ", x$user$email),
    paste0("caching         reason ", x$user$download_reason_id))

  package_lookup <- unlist(x$package[1:4]) |> as.integer() + 1
  names(package_settings) <- c("x", "v")[package_lookup]
  atlas_settings <- glue(" {x$atlas$organisation} ({x$atlas$acronym}) [{x$atlas$region}]")
  names(atlas_settings) <- ">"

  print_text <- c(
    crayon::bold("Package:          User:"),
    package_settings,
    "",
    crayon::bold("Atlas:"),
    atlas_settings
    )

  cat(format_error_bullets(print_text))
}