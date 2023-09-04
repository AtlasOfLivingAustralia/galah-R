#' Get or set configuration options that control galah behaviour
#'
#' The `galah` package supports large data downloads, and also
#' interfaces with the ALA which requires that users of some services
#' provide a registered email address and reason for downloading data. The
#' `galah_config` function provides a way to manage these issues as simply
#' as possible.
#'
#' @param profile_path 
#'    `r lifecycle::badge("deprecated")` 
#'    
#'    Keeping for compatibility with older package versions. It is preferable to 
#'    not save `galah_config` options to a .Rprofile file.
#' @param \dots Options can be defined using the form `name = "value"`.
#' Valid arguments are:
#' 
#'   *  `atlas` string: Living Atlas to point to, Australia by default. Can be 
#'   an organisation name, acronym, or region (see [show_all_atlases()] for 
#'   admissible values)
#'   *  `caching` logical: if TRUE, results will be cached, and any cached
#'     results will be re-used). If FALSE, data will be downloaded.
#'   *  `cache_directory` string: the directory to use for the cache.
#'     By default this is a temporary directory, which means that results will
#'     only be cached
#'     within an R session and cleared automatically when the user exits R.
#'     The user may wish to set this to a non-temporary directory for
#'     caching across sessions. The directory must exist on the file system.
#'   *  `download_reason_id` numeric or string: the "download reason" required.
#'   by some ALA services, either as a numeric ID (currently 0--13)
#'   or a string (see `show_all(reasons)` for a list of valid ID codes and
#'   names). By default this is NA. Some ALA services require a valid
#'   download_reason_id code, either specified here or directly to the
#'   associated R function.
#'   *  `email` string: An email address that has been registered with the chosen
#'   atlas. For the ALA, you can register at
#'   [this address](https://auth.ala.org.au/userdetails/registration/createAccount).
#'   *  `password` string: A registerd password (GBIF only)
#'   *  `run_checks` logical: should `galah` run checks for filters
#'   and columns. If making lots of requests sequentially, checks can slow down
#'   the process and lead to HTTP 500 errors, so should be turned off. Defaults
#'   to TRUE.
#'   *  `send_email` logical: should you receive an email for each query to
#'     [atlas_occurrences()]? Defaults to `FALSE`; but can be
#'     useful in some instances, for example for tracking DOIs assigned to
#'     specific downloads for later citation.
#'   *  `username` string: A registerd username (GBIF only)
#'   *  `verbose` logical: should `galah` give verbose such as progress bars?
#'  Defaults to FALSE.
#' 
#' @return For `galah_config()`, a `list` of all options.
#' When `galah_config(...)` is called with arguments, nothing is returned
#' but the configuration is set.
#' 
#' @examples 
#' \dontrun{
#' # To download occurrence records, enter your email in `galah_config()`. 
#' # This email should be registered with the ALA. 
#' # You can register at:
#' # https://auth.ala.org.au/userdetails/registration/createAccount
#' galah_config(email = "your-email@email.com")
#'  
#' # Turn on caching in your session
#' galah_config(caching = TRUE)
#'  
#' # Some ALA services require that you add a reason for downloading data. 
#' # Add your selected reason using the option `download_reason_id`
#' galah_config(download_reason_id = 0)
#' 
#' # To look up all valid reasons to enter, use `show_all(reasons)`
#' show_all(reasons)
#' 
#' # Make debugging in your session easier by setting `verbose = TRUE`
#' galah_config(verbose = TRUE)
#' }
#' @export galah_config

galah_config <- function(..., profile_path = NULL) {
  ala_option_name <- "galah_config"
  current_options <- getOption(ala_option_name)
  user_options <- list(...)

  if (length(user_options) == 0 && !is.null(current_options)) {
    return(current_options)
  }
  if (is.null(current_options)) {
    ## galah options have not been set yet, so set them to the defaults
    current_options <- default_config()
    if (!dir.exists(current_options$package$cache_directory)) {
      dir.create(current_options$package$cache_directory)
    }
    ## set the global option
    temp <- list(current_options)
    names(temp) <- ala_option_name
    class(temp) <- "galah_config"
    options(temp)
    return(current_options)
  } else {
    # check all the options are valid, if so, set as options

    if (!is.null(user_options$atlas)) {
      new_atlas <- configure_atlas(user_options$atlas)
      check_atlas(current_options$atlas, new_atlas) # see whether atlases have changed, and if so, give a message
      current_options$atlas <- new_atlas
      user_options <- user_options[names(user_options) != "atlas"]
    }

    if (!is.null(user_options$download_reason_id)) {
      user_options$download_reason_id <-
        convert_reason(user_options$download_reason_id)
    }

    for (x in names(user_options)) {
      validate_option(x, user_options[[x]])
      switch(x,
             # package
             "verbose" = {current_options$package$verbose <- user_options[[x]]},
             "run_checks" = {current_options$package$run_checks <- user_options[[x]]},
             "send_email" = {current_options$package$send_email <- user_options[[x]]},
             "caching" = {current_options$package$caching <- user_options[[x]]},
             "cache_directory" = {current_options$package$cache_directory <- user_options[[x]]},
             # user
             "username" = {current_options$user$username <- user_options[[x]]}, # gbif only
             "email" = {current_options$user$email <- user_options[[x]]},
             "password" = {current_options$user$password <- user_options[[x]]}, # gbif only
             "download_reason_id" = {current_options$user$download_reason_id <- user_options[[x]]})
    }

    ## set the global option
    temp <- list(current_options)
    names(temp) <- ala_option_name
    class(temp) <- "galah_config"
  }
  options(temp)

  # set profile  
  if (!is.null(profile_path)) {
    lifecycle::deprecate_soft(
      when = "1.4.1",
      what = "galah_config(profile_path = 'is soft-deprecated')",
      details = c(
        "*" = "We no longer recommend saving `galah_config()` options.",
        i = "Reloading config options improves code reproducibility.")
      )
    if (!file.exists(profile_path) || basename(profile_path) != ".Rprofile") {
      bullets <- c(
        glue("No .Rprofile file exists at \"{profile_path}\"."),
        i = "Please create fhe file and try again."
      )
      abort(bullets, call = caller_env())
    }
    if (current_options$verbose) {
      inform(glue("The config will be stored in {profile_path}."))
    }
    save_config(profile_path, current_options)

  }
}

default_config <- function(){
  x <- list(
    package = list( 
      verbose = TRUE,
      run_checks = TRUE,
      send_email = FALSE,
      caching = FALSE,
      cache_directory = tempdir()),
    user = list(
      username = "",
      email = "",
      password = "",
      download_reason_id = 4),
    atlas = list(
      organisation = "Atlas of Living Australia",
      acronym  = "ALA",
      region = "Australia"))
    class(x) <- "galah_config"
    x
}


configure_atlas <- function(query){
  
  comparison <- do.call(c, node_metadata)
  comparison <- comparison[!is.na(comparison)] |> as.character()
  lookup <- adist(query, comparison, ignore.case = TRUE)[1, ]
  
  if(all(lookup > 2)){
    bullets <- c(
      "Unsupported atlas provided.",
      i = glue("Use `show_all(atlases)` to see supported atlases."),
      x = glue("\"{query}\" is not a valid atlas.")
    )
    abort(bullets, call = caller_env())
  }else{
    selected_entry <- comparison[which(lookup == min(lookup))][[1]]
    
    # find appropriate row
    selected_row <- apply(node_metadata, 1, 
                          function(a){any(a == selected_entry, na.rm = TRUE)}) |>
      which()
    result <- list(
      organisation = node_metadata$institution[selected_row],
      acronym = node_metadata$acronym[selected_row],
      region = node_metadata$region[selected_row])
    
    return(result)
  }
}

check_atlas <- function(current, new){
  if(new$region != current$region){
    message(glue(
      "Atlas selected: {new$organisation} ({new$acronym}) [{new$region}]"))
  }
}
 

save_config <- function(profile_path, new_options) {
  if (!file.exists(profile_path)) {
    inform(glue(".Rprofile file doesn't exist yet. \\
                It will be created at \"{profile_path}\"."))
    file.create(profile_path)
    existing_options <- list()
    old_profile <- ""
  } else {
    # find existing options in file
    old_profile <- readLines(file.path(profile_path))
    # try one bracket and two brackets
    existing_options <- read_options(old_profile)
  }
  existing_options[["galah_config"]] <- new_options
  
  options_to_write <- paste0(
    "options(",paste(
      sapply(
        seq_len(length(existing_options)), function(x) {
          opt <- existing_options[[x]]
          opt_name <- names(existing_options)[[x]]
          if (is.list(opt)) {
            opts <- paste(sapply(seq_len(length(opt)), function(y) {
              paste(names(opt)[[y]], quoted_options(opt[[y]]), sep = " = ",
                    collapse = ", ")
            }), collapse = ", ")
            paste0(opt_name," = list(", opts, ")")
          } else {
            paste(opt_name, quoted_options(opt), sep = " = ", collapse = ",")
          }
        }),
      collapse = ","),
    ")")
  
  new_profile <- build_options(old_profile, options_to_write)
  
  con <- file(profile_path)
  writeLines(new_profile, con)
  close(con)
}

build_options <- function(old_profile, opts) {
  # if two brackets
  if (!is.na(str_match(old_profile, "options\\(\\s*(.*?)\\s*\\)\\)")[1])) {
    return(str_replace(old_profile, "options\\(\\s*(.*?)\\s*\\)\\)", opts))
  }
  # one bracket
  if (!is.na(str_match(old_profile, "options\\(\\s*(.*?)\\s*\\)")[1])) {
    return(str_replace(old_profile, "options\\(\\s*(.*?)\\s*\\)", opts))
  }
  # assume no match
  return(opts)
}


# function to read existing options file
# handles case when listed options already contains a nested list
# and when it doesn't
read_options <- function(profile) {
  # try two brackets
  opts <- str_match(profile, "options\\(\\s*(.*?)\\s*\\)\\)")[1]
  if (is.na(opts)) {
    # try one bracket
    opts <- str_match(profile, "options\\(\\s*(.*?)\\s*\\)")[1]
  }
  # no options exist
  if (is.na(opts)) {
    return(list())
  }
  eval(parse(text = opts))
}


quoted_options <- function(opts) {
  sapply(opts, function(x) {
    ifelse(is.logical(x), x, paste0("\"", x, "\""))
  })
}

validate_option <- function(name, value, error_call = caller_env()) {
  if (name %in% c("caching", "send_email", "verbose", "run_checks")) {
    if (!is.logical(value)) {
      abort(glue("\"{name}\" must be TRUE or FALSE."), call = error_call)
    }
  } else if (name == "cache_directory") {
    if (!dir.exists(value)) {
      bullets <- c(
        "Cache directory does not exist.",
        i = "Does the directory entered exist?"
      )
      abort(bullets, call = error_call)
    }
  } else if (name %in% c("email", "password", "username")) {
    if (!is.character(value)) {
      bullets <- c(
        glue("Invalid {name}"),
        i = "This argument must be entered as a string."
      )
      abort(bullets, call = error_call)
    }
  } else if (name == "download_reason_id") {
    if (!(value %in% show_all_reasons()$id)) {
      bullets <- c(
        "Invalid download reason ID or name.",
        i = "Use `show_all(atlases)` to see all valid reasons.",
        x = glue("{value} does not match an existing reason ID.")
      )
      abort(bullets, call = error_call)
    }
  } else {
    bullets <- c(
      "Invalid option name.",
      i = "See `?galah_config()` for valid options.",
      x = glue("\"{name}\" is not a valid option name.")
    )
    abort(bullets, call = error_call)
  }
}

convert_reason <- function(reason, error_call = caller_env()) {
  valid_reasons <- show_all_reasons()
  bullets <- c(
    "Invalid reason provided to `download_reason_id`.",
    i = "Use `show_all(reasons)` to see list of valid reasons.",
    x = glue("Couldn't match \"{reason}\" to a valid reason ID.")
  )
  ## unexported function to convert string reason to numeric id
  if (is.character(reason)) {
    tryCatch({
      reason <- match.arg(tolower(reason), valid_reasons$name)
      reason <- valid_reasons$id[valid_reasons$name == reason]
      },
      error = function(e) {abort(bullets, call = error_call)}
    )
  }else{
    if(is.numeric(reason)){
      if(reason > nrow(valid_reasons)){
        abort(bullets, call = error_call)
      }
    }
  }
  reason
}
