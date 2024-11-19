#' Print galah objects
#' 
#' As of version 2.0, `galah` supports several bespoke object types. Classes 
#' `data_request`, `metadata_request` and `files_request` are for starting pipes
#' to download different types of information. These objects are parsed using
#' `collapse()` into a `query` object, which contains one or more URLs necessary
#' to return the requested information. This object is then passed to
#' `compute()` and/or `collect()`. Finally, `galah_config()` creates an object
#' of class `galah_config` which (unsurprisingly) stores configuration 
#' information.
#' @name print_galah_objects
#' @param x an object of the appropriate `class`
#' @param ... Arguments to be passed to or from other methods
#' @returns Print does not return an object; instead it prints a description
#' of the object to the console
#' @examples \dontrun{
#' # The most common way to start a pipe is with `galah_call()`
#' # later functions update the `data_request` object
#' galah_call() |> # same as calling `request_data()`
#'   filter(year >= 2020) |>
#'   group_by(year) |>
#'   count()
#'
#' # Metadata requests are formatted in a similar way
#' request_metadata() |>
#'   filter(field == basisOfRecord) |>
#'   unnest()
#' 
#' # Queries are converted into a `query_set` by `collapse()`
#' x <- galah_call() |> # same as calling `request_data()`
#'   filter(year >= 2020) |>
#'   count() |>
#'   collapse()
#' print(x)
#'   
#' # Each `query_set` contains one or more `query` objects
#' x[[3]]
#' }
#' @export
print.data_request <- function(x, # NOTE: use of `x` arg here is for consistency with `print()`,
                               ...
                               ){
  format_request_text(x, 
                      object_type = "data_request")
}

#' @rdname print_galah_objects
#' @export
print.files_request <- function(x, ...){
  format_request_text(x, 
                      object_type = "files_request")
}

#' @rdname print_galah_objects
#' @export
print.metadata_request <- function(x, ...){
  format_request_text(x, 
                      object_type = "metadata_request")
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
    "data_profile" ={x[[a]][1]},
    "mint_doi" = {x[[a]][1]},
    "")
}

#' @rdname print_galah_objects
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
      url_temp <- x$url$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      if(nrow(x$url) > 1){
        url_text <- glue("\n
           url: {url_temp} + {nrow(x$url) - 1} more") 
      }else{
        url_text <-glue("\n
           url: {url_temp}")
      }
    }else{
      url_temp <- x$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      url_text <-glue("\n
           url: {url_temp}")
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

#' @rdname print_galah_objects
#' @export
print.computed_query <- function(x, ...){
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
      url_temp <- x$url$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      if(nrow(x$url) > 1){
        url_text <- glue("\n
           url: {url_temp} + {nrow(x$url) - 1} more") 
      }else{
        url_text <-glue("\n
           url: {url_temp}")
      }
    }else{
      url_temp <- x$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      url_text <-glue("\n
           url: {url_temp}")
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
    galah_pink("computed_query"),
    silver("with type"),
    galah_green(x$type),
    subtext, # note: need code for url tibbles
    arrange,
    slice))
}

#' @rdname print_galah_objects
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
      url_temp <- a$url[1]
      pretext_length <- nchar(a$type) + 6
      if(sum(c(pretext_length, nchar(url_temp))) > 80){
        url_temp <- paste0(substr(url_temp, 1, (80 - pretext_length - 3)), "...")
      }
      subtext <- galah_grey(glue("url: {url_temp}"))
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

#' @rdname print_galah_objects
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
    "{galah_green('download_reason_id')}   {galah_grey(x$user$download_reason_id)}")
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
