#' Get or set configuration options that control galah behaviour
#'
#' The \code{galah} package supports large data downloads, and also
#' interfaces with the ALA which requires that users of some services
#' provide a registered email address and reason for downloading data. The
#' \code{ala_config} function provides a way to manage these issues as simply
#' as possible.
#'
#' @param profile_path string: (optional), path to a directory to store
#' config values in. If provided, config values will be written to a new or
#' existing .Rprofile file for future sessions. \code{NULL} by default.
#' @param \dots Options can be defined using the form \code{name = value}.
#' Valid arguments are:
#' \itemize{
#'   \item \code{atlas} string: Living Atlas to point to, Australia by default
#'   \item \code{caching} logical: if TRUE, results will be cached, and any cached
#'     results will be re-used). If FALSE, data will be downloaded.
#'   \item \code{cache_directory} string: the directory to use for the cache.
#'     By default this is a temporary directory, which means that results will
#'     only be cached
#'     within an R session and cleared automatically when the user exits R.
#'     The user may wish to set this to a non-temporary directory for
#'     caching across sessions. The directory must exist on the file system.
#'   \item \code{download_reason_id} numeric or string: the "download reason" required.
#'   by some ALA services, either as a numeric ID (currently 0--11)
#'   or a string (see \code{\link{find_reasons}()} for a list of valid ID codes and
#'   names). By default this is NA. Some ALA services require a valid
#'   download_reason_id code, either specified here or directly to the
#'   associated R function.
#'   \item \code{email} string: An email address that has been registered with
#'     ALA at \href{https://auth.ala.org.au/userdetails/registration/createAccount}{this address}.
#'     A registered email is required for some functions in \code{galah}.
#'   \item \code{send_email} logical: should you receive an email for each query to
#'     \code{\link{ala_occurrences}()}? Defaults to \code{FALSE}; but can be
#'     useful in some instances, for example for tracking DOIs assigned to
#'     specific downloads for later citation.
#'   \item \code{verbose} logical: should \code{galah} give verbose output to assist
#'   debugging? Defaults to FALSE.
#' }
#'
#' @return For \code{ala_config()}, a \code{list} of all options.
#' When \code{ala_config(...)} is called with arguments, nothing is returned
#' but the configuration is set.
#'
#' @examples
#' \dontrun{
#'  ala_config()
#'  ala_config(caching = FALSE)
#'  ala_reasons()
#'  ala_config(download_reason_id = 0,verbose = TRUE)
#' }
#' @export ala_config

ala_config <- function(..., profile_path = NULL) {
  ala_option_name <- "galah_config"
  current_options <- getOption(ala_option_name)

  user_options <- list(...)

  default_options <- list(
    caching = FALSE,
    cache_directory = tempdir(),
    atlas = "Australia",
    download_reason_id = 4,
    email = "",
    send_email = FALSE,
    verbose = TRUE
  )

  if (length(user_options) == 0 && !is.null(current_options)) {
    return(current_options)
  }
  if (is.null(current_options)) {
    ## galah options have not been set yet, so set them to the defaults
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


  if (!is.null(profile_path)) {
    if (!file.exists(profile_path) || basename(profile_path) != ".Rprofile") {
      stop("No .Rprofile file exists at '", profile_path,
           "' . Please create the file and try again.")
    }
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

validate_option <- function(name, value) {
  if (name == "caching") {
    if (!is.logical(value)) {
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
    if (!(value %in% find_reasons()$id)) {
      stop("Download reason must be a valid reason id or name ",
           "See `find_reasons()` for valid reasons.")
    }
  } else if (name == "atlas") {
    if (!value %in% find_atlases()$atlas) {
      stop("Atlas must be one of ",
           paste(find_atlases()$atlas, collapse = ", "))
    }
  } else {
    stop("\"", name, "\"", "is not a valid option name.")
  }
}

#' List valid download reasons
#'
#' When downloading occurrence data with \code{\link{ala_occurrences}} the
#' ALA APIs require a reason for download to be specified. By default, a
#' download reason of 'scientific research' is set for you, but if you wish to
#' change this you can do so with \code{\link{ala_config}()}. Use this function
#' to view the list of download reason code and names. When specifying a reason,
#' you can use either the download code or name.
#' @rdname find_reasons
#' @seealso This function is helpful in setting up \code{\link{ala_config}()}.
#' @return A \code{data.frame} of valid download reasons, containing the id
#' and name for each reason.
#' @export
find_reasons <- function() {
    ## return list of valid "reasons for use" codes
    out <- ala_GET(server_config("logger_base_url"),
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
    valid_reasons <- find_reasons()
    tryCatch({
      reason <- match.arg(tolower(reason), valid_reasons$name)
      reason <- valid_reasons$id[valid_reasons$name == reason]
    },
    error = function(e) {
      stop("could not match download_reason_id string \"", reason,
           "\" to valid reason id: see find_reasons() for valid reasons")
    })
  }
  reason
}

