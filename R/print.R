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
  filled_slots <- !unlist(purrr::map(x, is.null))
  if(any(filled_slots)){
    cli::cli_text("Object of type {galah_pink(object_type)} containing:")
    x_names <- names(x)[filled_slots]
    purrr::map(x_names, 
               function(a){
                 slot_name <- galah_green(a)
                 if(a == "filter"){
                   slot_content <- utils::capture.output(print(x[[a]])) # calls `print.data_filter()` etc.
                 }else{
                   slot_content <- switch_slot_text(x, a)
                 }
                 glue::glue("{slot_name} {galah_grey(slot_content)}") 
             }) |>
      cli::cli_li()
  }else{
    cli::cli_text("An empty object of type {galah_pink(object_type)}")
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
        glue::glue("{first_col}s: {first_entry} + {n_entries - 1} more")  
      }else{
        glue::glue("{first_col}: {first_entry}")
      }
    },
    "select" = x[[a]]$summary,
    "group_by" =  glue::glue_collapse(x[[a]]$name, sep = " | "),
    "apply_profile" ={x[[a]][1]},
    "mint_doi" = {x[[a]][1]},
    "")
}

#' @rdname print_galah_objects
#' @export
print.query <- function(x, ...){
  if(!is.null(x$arrange)){
    arrange <- galah_pale_green(glue::glue("\n
                              arrange: {x$arrange$variable} ({x$arrange$direction})"))
    if(x$arrange$slice_called == TRUE){
      slice <- galah_pale_green(glue::glue("\n
                              slice: {x$arrange$slice_n}"))
    }else{
      slice <- NULL
    }
  }else{
    arrange <- NULL
    slice <- NULL
  }
  if(!is.null(x$url)){
    if(inherits(x$url, "data.frame")){
      url_temp <- x$url$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      if(nrow(x$url) > 1){
        url_text <- glue::glue("\n
           url: {url_temp} + {nrow(x$url) - 1} more") 
      }else{
        url_text <- glue::glue("\n
           url: {url_temp}")
      }
    }else{
      url_temp <- x$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      url_text <- glue::glue("\n
           url: {url_temp}")
    }
    subtext <- galah_grey(url_text)
  }else if(!is.null(x$data)){
    subtext <- galah_grey(glue::glue("\n
                               data: {x$data[1]}"))
  }else if(!is.null(x$status)){
    subtext <- galah_grey(glue::glue("\n
                               status: {x$status[1]}"))
  }else{
    subtext <- NULL
  }
  if(!is.null(x$select)){
    select <- galah_grey(glue::glue("\n
                               select: {x$select$summary}"))
  }else{
    select <- NULL
  }
  
  # keep only populated levels
  print_list <-  list(subtext, # note: need code for url tibbles
                      select,
                      arrange,
                      slice)
  print_list <- print_list[!unlist(purrr::map(print_list, is.null))]

  # print
  class_tr <- class(x)[1]
  cli::cli_text("Object of class {galah_pink(class_tr)} with type {galah_green(x$type)}")
  cli::cli_li(print_list)
}

#' @rdname print_galah_objects
#' @export
print.prequery <- print.query

#' @rdname print_galah_objects
#' @export
print.computed_query <- function(x, ...){
  # calculate arrange/slice info
  if(!is.null(x$arrange)){
    arrange <- galah_pale_green(glue("\n
                              arrange: {x$arrange$variable} ({x$arrange$direction})"))
    if(x$arrange$slice_called == TRUE){
      slice <- galah_pale_green(glue("\n
                              slice: {x$arrange$slice_n}"))
    }else{
      slice <- NULL
    }
  }else{
    arrange <- NULL
    slice <- NULL
  }
  
  # add url
  if(!is.null(x$url)){
    if(inherits(x$url, "data.frame")){
      url_temp <- x$url$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      if(nrow(x$url) > 1){
        url_text <- glue::glue("\n
           url: {url_temp} + {nrow(x$url) - 1} more") 
      }else{
        url_text <- glue::glue("\n
           url: {url_temp}")
      }
    }else{
      url_temp <- x$url[1]
      if(nchar(url_temp) > 70){
        url_temp <- paste0(substr(url_temp, 1, 70), "...")
      }
      url_text <- glue::glue("\n
           url: {url_temp}")
    }
    subtext <- galah_grey(url_text)
  }else if(!is.null(x$data)){
    subtext <- galah_grey(glue::glue("\n
                               data: {x$data[1]}"))
  }else if(!is.null(x$status)){
    subtext <- galah_grey(glue::glue("\n
                               status: {x$status[1]}"))
  }else{
    subtext <- NULL
  }
  
  # add a status ID
  if(!is.null(x$status_url)){
    split_url <- strsplit(x$status_url, "\\/")[[1]] 
    id <- galah_grey(glue::glue("\n
                                id: {split_url[[length(split_url)]]}"))
  }else{
    id <- NULL
  }
  
  # keep only populated levels
  print_list <-  list(id,
                      subtext, # note: need code for url tibbles
                      arrange,
                      slice)
  print_list <- print_list[!unlist(purrr::map(print_list, is.null))]
  
  # print
  cli::cli_text("Object of class {galah_pink(\"computed query\")} with type {galah_green(x$type)}")
  cli::cli_li(print_list)
}

#' @rdname print_galah_objects
#' @export
print.query_set <- function(x, ...){
  n_queries <- length(x)
  header_text <- c("Object of class ",
                   galah_pink("query_set "),
                   "containing ",
                   ifelse(n_queries > 1, 
                          "{n_queries} queries:",
                          "1 query:"))
  cli::cli_text(header_text)
  purrr::map(x, function(a){
    type_text <- galah_green(a$type)
    if(!is.null(a$url)){
      url_temp <- a$url[1]
      pretext_length <- nchar(a$type) + 6
      if(sum(c(pretext_length, nchar(url_temp))) > 80){
        url_temp <- paste0(substr(url_temp, 1, (80 - pretext_length - 3)), "...")
      }
      subtext <- galah_grey(glue::glue("url: {url_temp}"))
    }else if(!is.null(a$data)){
      subtext <- galah_grey(glue::glue("data: {a$data[1]}"))
    }else{
      subtext <- ""
    } 
    glue::glue("{type_text} {subtext}")
  }) |>
    cli::cli_li()
}

#' @rdname print_galah_objects
#' @export
print.galah_config <- function(x, ...){
  cli::cli_par()
  cli::cli_text("`galah` package configuration")
  cli::cli_end()
  cli::cli_par()
  # print package settings
  cli::cli_text("{galah_pink(\"Package\")}")
  package_info <- purrr::pluck(x, "package")
  package_logical_check <- purrr::map(package_info, is.logical) |>
    unlist()
  logical_values <- package_info[package_logical_check] |>
    unlist()
  package_settings <- names(logical_values) |>
    galah_green()
  names(package_settings) <- c("x", "v")[as.integer(logical_values) + 1]
  package_settings <- c(package_settings,
                        "i" = glue::glue("{galah_green('directory')}: {galah_grey(x$package$directory)}")) |>
    cli::cli_bullets()
  cli::cli_end()
  # print user settings
  cli::cli_par()
  cli::cli_text("{galah_pink(\"User\")}")
  user_settings <- c(
    "{galah_green('authentication')}",
    "{galah_green('username')} {galah_grey(hide_secrets(x$user$username))}",
    "{galah_green('email')}    {galah_grey(x$user$email)}",
    "{galah_green('password')} {galah_grey(hide_secrets(x$user$password))}",
    "{galah_green('download_reason_id')}   {galah_grey(x$user$download_reason_id)}")
  names(user_settings)[1] <- purrr::pluck(x, "user", "authenticate") |>
    isTRUE() |>
    ifelse("v", "x")
  cli::cli_bullets(user_settings)
  cli::cli_end()
  cli::cli_par()
  cli::cli_text("{galah_pink(\"Atlas\")}")
  atlas_text <- galah_green(x$atlas$organisation)
  atlas_subtext <- galah_grey(glue::glue("({x$atlas$acronym}), {x$atlas$region}"))
  cli::cli_bullets("{atlas_text} {atlas_subtext}")
  cli::cli_end()
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
#' @noRd
#' @keywords Internal
galah_pink <- crayon::make_style("#bf2a6d")

#' Green for printing secondary text (e.g. object types) to the console
#' @noRd
#' @keywords Internal
galah_green <- crayon::make_style("#176666")

#' Green for printing non-emphasized text to the console
#' @noRd
#' @keywords Internal
galah_pale_green <- crayon::make_style("#60a3a3")

#' Grey for printing non-emphasized text (e.g. urls) to the console
#' @noRd
#' @keywords Internal
galah_grey <- crayon::make_style("#8c8c8c")
