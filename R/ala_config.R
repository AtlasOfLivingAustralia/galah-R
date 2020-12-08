#' Get or set configuration options that control ALA4R behaviour
#'
#' @references \url{https://api.ala.org.au/}
#' @references \url{https://spatial.ala.org.au/layers-service/} this will
#' eventually move to the api link
#'
#' Invoking \code{ala_config()} with no arguments returns a list with the
#' current values of the options.
#'
#' \code{ala_reasons()} returns a data frame with information describing the
#' valid options for \code{download_reason_id}
#' 
#' @param preserve logical: store config for future sessions?
#'
#' @param \dots Options can be defined using name = value. Valid options are:
#' \itemize{
#'   \item reset: \code{ala_config("reset")} will reset the options to their
#'   default values
#'   \item caching string: caching can be
#'     "on" (results will be cached, and any cached results will be re-used),
#'     "refresh" (cached results will be refreshed and the new results stored
#'     in the cache), or
#'     "off" (no caching, default).
#'   \item cache_directory string: the directory to use for the cache.
#'     By default this is a temporary directory, which means that results will
#'     only be cached
#'     within an R session and cleared automatically when the user exits R.
#'     The user may wish to set this to a non-temporary directory for
#'     caching across sessions. The directory must exist on the file system.
#'   \item verbose logical: should ALA4R give verbose output to assist
#'   debugging? (default = FALSE)
#'   \item warn_on_empty logical: should a warning be issued if a request
#'   returns an empty result set? (default = FALSE)
#'   \item user_agent string: the user-agent string used with all web requests
#'   to the ALA servers.
#'     Default = "ALA4R" with version number
#'   \item text_encoding string: text encoding assumed when reading cached
#'   files from local disk (default = "UTF-8")
#'   \item download_reason_id numeric or string: the "download reason" required
#'   by some ALA services, either as a numeric ID (currently 0--11)
#'   or a string (see \code{ala_reasons()} for a list of valid ID codes and
#'   names). By default this is NA. Some ALA services require a valid
#'   download_reason_id code, either specified here or directly to the
#'   associated R function.
#' }
#'
#' @return For ala_config(), a list of all options. When ala_config(...) is
#' called with arguments, nothing is returned but the configuration is set.
#'
#' @examples
#' \dontrun{
#'  ala_config()
#'  ala_config(caching = "off")
#'  ala_reasons()
#'  ala_config(download_reason_id = 0,verbose = TRUE)
#'  ala_config("reset")
#' }
#' @export ala_config

ala_config <- function(..., preserve = FALSE) {
  ala_option_name <- "ALA4R_config"
  current_options <- getOption(ala_option_name)
  
  assert_that(is.logical(preserve))
  user_options <- list(...)
  
  default_options <- list(
    caching = FALSE,
    cache_directory = tempdir(),
    download_reason_id = 4,
    email = "",
    send_email = FALSE,
    verbose = FALSE
  )
  
  if (length(user_options) == 0 && !is.null(current_options)) {
    return(current_options)
  }
  if (is.null(current_options)) {
    ## ALA4R options have not been set yet, so set them to the defaults
    current_options <- default_options
    if (!dir.exists(current_options$cache_directory)) {
      dir.create(current_options$cache_directory)
    }
    ## set the global option
    temp <- list(current_options)
    names(temp) <- ala_option_name
    options(temp)
    return(current_options)
  } else {
    # check all the options are valid, if so, set as options
    
    if (!is.null(user_options$download_reason_id)) {
      user_options$download_reason_id <-
        convert_reason(user_options$download_reason_id)
    }
    
    for (x in names(user_options)) {
      validate_option(x, user_options[[x]])
      current_options[[x]] <- user_options[[x]]
    }
    
    ## set the global option
    temp <- list(current_options)
    names(temp) <- ala_option_name
  }
  options(temp)
  
  
  if (preserve) {
    profile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")
    if (current_options$verbose) {
      message("The config will be stored in ", profile_path)
    }
    save_config(profile_path, current_options)
    
  } else {
    if (current_options$verbose) {
      msg <- "These configuration options will only be saved for this session. 
    Set `preserve = TRUE` to preserve them for future sessions."
    }
  }
}

save_config <- function(profile_path, new_options) {
  if (!file.exists(profile_path)) {
    message(".Rprofile file doesn't exist yet. It will be created at ",
            profile_path)
    file.create(profile_path)
    existing_options <- list()
    old_profile <- ""
  } else {
    # find existing options in file
    old_profile <- readLines(file.path(profile_path))
    # try one bracket and two brackets
    existing_options <- read_options(old_profile)
  }
  existing_options[["ALA4R_config"]] <- new_options
  
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

validate_option <- function(name, value) {
  if (name == "caching") {
    if (!is.logical(value) && !(value %in% c("on", "off", "reset"))) {
      stop("\"", name, "\"", " must be TRUE or FALSE")
    }
  } else if (name == "send_email" || name == "verbose") {
    if (!is.logical(value)) {
      stop("\"", name, "\"", " must be TRUE or FALSE")
    }
  } else if (name == "cache_directory") {
    if (!dir.exists(value)) {
      stop("Cache directory does not exist, please create it and try again.")
    }
  } else if (name == "email") {
    if (!is.character(value)) {
      stop("Email must be a string")
    }
  } else if (name == "download_reason_id") {
    if (!(value %in% ala_reasons()$id)) {
      stop("Download reason must be a valid reason id or name ",
           "See `ala_reasons()` for valid reasons.")
    }
  } else {
    stop("\"", name, "\"", "is not a valid option name.")
  }
}

#' @rdname ala_config
#' @export
ala_reasons <- function() {
    ## return list of valid "reasons for use" codes
    out <- ala_GET(getOption("ALA4R_server_config")$base_url_logger,
                           path = "service/logger/reasons")
    if (any(names(out) == "deprecated")) out <- out[!out$deprecated, ]
    out <- out[wanted_columns("reasons")]
    # sort by id to make it less confusing
    row.names(out) <- out$id
    out[order(out$id),]
}

convert_reason <- function(reason) {
  ## unexported function to convert string reason to numeric id
  if (is.character(reason)) {
    valid_reasons <- ala_reasons()
    tryCatch({
      reason <- match.arg(tolower(reason), valid_reasons$name)
      reason <- valid_reasons$id[valid_reasons$name == reason]
    },
    error = function(e) {
      stop("could not match download_reason_id string \"", reason,
           "\" to valid reason id: see ala_reasons() for valid reasons")
    })
  }
  reason
}
