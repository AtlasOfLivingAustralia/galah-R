#' @rdname galah_call
#' @param x an object of class `data_request`
#' @export
print.data_request <- function(x, ...){
  filled_slots <- !unlist(lapply(x, is.null))
  if(any(filled_slots)){
    inform(glue("Object of type {galah_pink('`data_request`')} containing:"))
    x_names <- names(x)[filled_slots]
    lapply(x_names, function(a){
      slot_name <- galah_green(a)
      slot_content <- switch(a,
                             "type" = x[[a]],
                             "identify" = {
                               first_col <- colnames(x[[a]])[1]
                               first_entry <- x[[a]][[1]][1]
                               n_entries <- nrow(x[[a]])
                               if(n_entries > 1){
                                 glue("{first_col}s: {first_entry} + {n_entries - 1} more")  
                               }else{
                                 glue("{first_col}: {first_entry}")
                               }
                             },
                             "filter" = glue_collapse(x[[a]]$query, 
                                                      sep = " | "),
                             "select" = glue_collapse(x[[a]]$name, 
                                                      sep = " | "),
                             "group_by" =  glue_collapse(x[[a]]$name, 
                                                         sep = " | "),
                             "")
      glue("{slot_name} {galah_grey(slot_content)}")
    }) |>
      unlist() |>
      format_error_bullets() |>
      cat()
  }else{
    cat("An empty object of type `data_request`")
  }
}
# NOTE: use of `x` arg here is for consistency with `print()`; do not change

#' @rdname galah_call
#' @param x an object of class `query`
#' @importFrom crayon silver
#' @export
print.query <- function(x, ...){
  if(!is.null(x$arrange)){
    arrange <- galah_pale_green(glue("\n
                              arrange: {x$arrange$variable} ({x$arrange$direction})"))
    if(x$arrange$slice_called == TRUE){
      slice <- galah_pale_green(glue("\n
                              slice: {x$arrange$slice_n}"))
    }else{
      slice <- ""
    }
  }else{
    arrange <-""
    slice <- ""
  }
  if(!is.null(x$url)){
    subtext <- galah_grey(glue("\n
                               url: {x$url[1]}"))
  }else if(!is.null(x$data)){
    subtext <- galah_grey(glue("\n&nbsp;&nbsp;
                               data: {x$data[1]}"))
  }else{
    ""
  }
  cat(c(
    silver("Object of class "),
    galah_pink("`query`"),
    silver(glue(" with type `{galah_green(x$type)}`")),
    subtext, # note: need code for url tibbles
    arrange,
    slice))
}

#' @rdname galah_call
#' @param x an object of class `query_set`
#' @export
print.query_set <- function(x, ...){
  n_queries <- length(x)
  message(c(silver("Object of class "),
        galah_pink("`query_set` "),
        silver(glue("containing ")),
        ifelse(n_queries > 1, 
               silver(glue("{n_queries} queries:")),
               silver("1 query:"))))
  lapply(x, function(a){
    type_text <- galah_green(a$type)
    if(!is.null(a$url)){
      subtext <- galah_grey(glue("url: {a$url[1]}"))
    }else if(!is.null(a$data)){
      subtext <- galah_grey(glue("data: {a$data[1]}"))
    }else{
      subtext <- ""
    } 
    glue("{type_text} {subtext}")
  }) |>
    unlist() |>
    format_error_bullets() |>
    cat()
}

#' @rdname galah_config
#' @param x an object of class `galah_config`
#' @importFrom rlang format_error_bullets
#' @export
print.galah_config <- function(x, ...){
  inform(galah_pink("Package"))
  package_settings <- galah_green(c("verbose", "run_checks", "send_email"))
  package_lookup <- unlist(x$package[1:3]) |> as.integer() + 1
  names(package_settings) <- c("x", "v")[package_lookup]
  package_settings <- c(package_settings,
                        "i" = glue("{galah_green('directory')}: {galah_grey(x$package$directory)}")) |>
    format_error_bullets() |>
    cat()
  cat("\n")
  inform(galah_pink("User"))         
  values <- c(
    "{galah_green('username')} {galah_grey(hide_secrets(x$user$username))}",
    "{galah_green('email')}    {galah_grey(x$user$email)}",
    "{galah_green('password')} {galah_grey(hide_secrets(x$user$password))}",
    "{galah_green('api_key')}  {galah_grey(hide_secrets(x$user$api_key))}",
    "{galah_green('reason')}   {galah_grey(x$user$download_reason_id)}")
  password_settings <- lapply(values, 
                              function(a, x){glue_data(x, a)}, x = x) |>
    unlist() |>
    format_error_bullets() |>
    cat()
  cat("\n")
  inform(galah_pink("Atlas"))
  atlas_text <- galah_green(x$atlas$organisation)
  atlas_subtext <- galah_grey(glue("({x$atlas$acronym}), {x$atlas$region}"))
  atlas_settings <- glue("{atlas_text} {atlas_subtext}") |>
    format_error_bullets() |>
    cat()
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

#' Pink for printing primary text (e.g. object names) to the console
#' @importFrom crayon make_style
#' @noRd
#' @keywords Internal
galah_pink <- make_style("#bf2a6d")

#' Green for printing secondary text (e.g. object types) to the console
#' @importFrom crayon make_style
#' @noRd
#' @keywords Internal
galah_green <- make_style("#176666")

#' Green for printing non-emphasized text to the console
#' @importFrom crayon make_style
#' @noRd
#' @keywords Internal
galah_pale_green <- make_style("#60a3a3")

#' Grey for printing non-emphasized text (e.g. urls) to the console
#' @importFrom crayon make_style
#' @noRd
#' @keywords Internal
galah_grey <- make_style("#8c8c8c")