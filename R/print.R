#' @rdname galah_call
#' @param x an object of class `data_request`
#' @export
print.data_request <- function(x, # NOTE: use of `x` arg here is for consistency with `print()`,
                               ...
                               ){
  format_request_text(x, 
                      object_type = "data_request")
}

#' @rdname galah_call
#' @param x an object of class `files_request`, created using `request_files()`
#' @export
print.files_request <- function(x, ...){
  format_request_text(x, 
                      object_type = "files_request")
}

#' @rdname galah_call
#' @param x an object of class `metadata_request`, created using `request_metadata()`
#' @export
print.metadata_request <- function(x, ...){
  format_request_text(x, 
                      object_type = "metadata_request")
}

#' @rdname galah_call
#' @param x an object of class `values_request`, created using `request_values()`
#' @export
print.values_request <- function(x, ...){
  format_request_text(x, 
                      object_type = "values_request")
}

#' Internal function to format text in `print()`
#' @noRd
#' @keywords Internal
format_request_text <- function(x, object_type){
  filled_slots <- !unlist(lapply(x, is.null))
  formatted_object <- galah_pink(glue("`{object_type}`"))
  if(any(filled_slots)){
    inform(glue("Object of type {formatted_object} containing:"))
    x_names <- names(x)[filled_slots]
    lapply(x_names, function(a){
      slot_name <- galah_green(a)
      slot_content <- switch_slot_text(x, a)
      glue("{slot_name} {galah_grey(slot_content)}")
    }) |>
      unlist() |>
      format_error_bullets() |>
      cat()
  }else{
    inform(glue("An empty object of type {formatted_object}"))
  }
}

#' Internal function to write pretty text for `data_request`s and related methods
#' @noRd
#' @keywords Internal
switch_slot_text <- function(x, a){
  switch(
    a,
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
    "filter" = {
      if(ncol(x[[a]]) > 2){
        df <- x[[a]][, 1:3]
      }else{
        df <- x[[a]]
      }
      if(nrow(df) > 1){
        df <- df[1, ]  
      }
      glue_collapse(
        apply(df, 1, function(b){paste(b, collapse = " ")}),
        sep = " | ")
    },
    "select" = x[[a]]$summary,
    "group_by" =  glue_collapse(x[[a]]$name, sep = " | "),
    "")
}

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
    if(inherits(x$url, "data.frame")){
      if(nrow(x$url) > 1){
        url_text <-glue("\n
           url: {x$url$url[1]} + {nrow(x$url) - 1} more") 
      }else{
        url_text <-glue("\n
           url: {x$url$url[1]}")
      }
    }else{
      url_text <-glue("\n
           url: {x$url[1]}")
    }
    subtext <- galah_grey(url_text)
  }else if(!is.null(x$data)){
    subtext <- galah_grey(glue("\n
                               data: {x$data[1]}"))
  }else if(!is.null(x$status)){
    subtext <- galah_grey(glue("\n
                               status: {x$status[1]}"))
  }else{
    subtext <- ""
  }
  cat(c(
    silver("Object of class"),
    galah_pink("query"),
    silver("with type"),
    galah_green(x$type),
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