#' Get or set configuration options that control galah behaviour
#'
#' The `galah` package supports large data downloads, and also
#' interfaces with the ALA which requires that users of some services
#' provide a registered email address and reason for downloading data. The
#' `galah_config` function provides a way to manage these issues as simply
#' as possible.
#'
#' @param \dots Options can be defined using the form `name = "value"`.
#' Valid arguments are:
#' 
#'   *  `api-key` string: A registered API key (currently unused). 
#'   *  `atlas` string: Living Atlas to point to, Australia by default. Can be 
#'   an organisation name, acronym, or region (see [show_all_atlases()] for 
#'   admissible values)
#'   *  `directory` string: the directory to use for the cache.
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
#'   *  `password` string: A registered password (GBIF only)
#'   *  `run_checks` logical: should `galah` run checks for filters
#'   and columns. If making lots of requests sequentially, checks can slow down
#'   the process and lead to HTTP 500 errors, so should be turned off. Defaults
#'   to TRUE.
#'   *  `send_email` logical: should you receive an email for each query to
#'     [atlas_occurrences()]? Defaults to `FALSE`; but can be
#'     useful in some instances, for example for tracking DOIs assigned to
#'     specific downloads for later citation.
#'   *  `username` string: A registered username (GBIF only)
#'   *  `verbose` logical: should `galah` give verbose such as progress bars?
#'  Defaults to FALSE.
#' 
#' @return For `galah_config()`, a `list` of all options.
#' When `galah_config(...)` is called with arguments, nothing is returned
#' but the configuration is set.
#' 
#' @examples \dontrun{
#' # To download occurrence records, enter your email in `galah_config()`. 
#' # This email should be registered with the atlas in question. 
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
#' @importFrom lifecycle deprecate_warn
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom potions brew
#' @importFrom potions pour
#' @export galah_config

galah_config <- function(...) {
  
  # make sure dots are captured properly
  dots <- list(...)

  # set defaults, if this has not happened already
  if(length(pour()) == 0) {
    brew(default_config())
  }
  
  # add user-provided information
  if(length(dots) > 0){
    
    # check for deprecated `cache_directory`
    if(any(names(dots) == "cache_directory")){
      dots_location <- which(names(dots) == "cache_directory")
      value <- dots$cache_directory
      deprecate_warn(when = "2.0.0",
                     what = "galah_config(cache_directory)",
                     details = glue("Use `galah_config(directory = \"{value}\")` instead.")
      )
      names(dots)[dots_location] <- "directory"
    }
    
    # check all values in dots to ensure they are named
    if(length(dots) != length(names(dots))){
      bullets <- c("All arguments to `galah_config() must be named.",
                   i = "Did you use `==` instead of `=`?")
      abort(bullets)
    }
    
    # check all values in dots to ensure they are valid
    result <- restructure_config(dots)

    # add to `potions` object
    if(any(names(result) == "atlas")){
      brew(atlas = list(atlas = result$atlas))
      result <- result[names(result) != "atlas"]
      result$atlas_config_called_by_user <- TRUE
    }
    
    if(length(result) > 0){
      brew(result, method = "leaves")
    }
  
  }else{
    x <- pour()
    class(x) <- c("galah_config", "list")
    return(x)
  }
}

#' Set a 'default' object for storing config in `galah`
#' @noRd
#' @keywords Internal
default_config <- function(){
  x <- list(
    package = list( 
      verbose = TRUE,
      run_checks = TRUE,
      send_email = FALSE,
      directory = tempdir(),
      atlas_config_called_by_user = FALSE),
    user = list(
      username = "",
      email = "",
      api_key = "",
      password = "",
      download_reason_id = 4),
    atlas = list(
      organisation = "Atlas of Living Australia",
      acronym  = "ALA",
      region = "Australia"))
  class(x) <- c("galah_config", "list")
  x
}

#' Place new options into correctly nested structure
#' @noRd
#' @keywords Internal
restructure_config <- function(dots){
  result <- lapply(names(dots),
         function(a){validate_config(a, dots[[a]])})
  names(result) <- names(dots)
  result
}

#' Catch errors in user-provided config
#' @importFrom rlang abort
#' @importFrom glue glue
#' @importFrom potions pour
#' @noRd
#' @keywords Internal
validate_config <- function(name, value, error_call = caller_env()) {
  result <- switch(name, 
         "api_key"         = enforce_character(value),
         "atlas" = {
           value <- configure_atlas(value)
           # see whether atlases have changed, and if so, give a message
           check_atlas(pour("atlas"), value)
         },
         "caching"         = enforce_logical(value),
         "directory"       = check_directory(value),
         "download_reason_id" = enforce_download_reason(value),
         "email"           = enforce_character(value),
         "password"        = enforce_character(value),
         "run_checks"      = enforce_logical(value),
         "send_email"      = enforce_logical(value),
         "username"        = enforce_character(value),
         "verbose"         = enforce_logical(value),
         enforce_invalid_name(name))
  result
}

#' Ensure some arguments are logical
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
enforce_logical <- function(value, error_call = caller_env()){
  if (!is.logical(value)) {
    abort("Supplied value must be TRUE or FALSE.", call = error_call)
  }else{
    value
  }
}

#' Ensure a file exists
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
enforce_exists <- function(value, error_call = caller_env()){
  if (!dir.exists(value)) {
    bullets <- c("Cache directory does not exist.",
                 i = "Does the directory entered exist?")
    abort(bullets, call = error_call)
  }else{
    value
  }
}

#' Ensure provided value is a string
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
enforce_character <- function(value, error_call = caller_env()){
  if (!is.character(value)) {
    bullets <- c(
      glue("Invalid type"),
      i = "Value must be entered as a string."
    )
    abort(bullets, call = error_call)
  }else{
    value
  }
}

#' Ensure download reason is valid
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
enforce_download_reason <- function(value, error_call = caller_env()){
  # first ensure API is available. Currently missing for Brazil, for example.
  
  reasons_api_available <- url_lookup("metadata/reasons") |> 
    try(silent = TRUE)
  if(inherits(reasons_api_available, "try-error")){
    return(1)
  }else{
    if (is.numeric(value) & !(value %in% show_all_reasons()$id)) {
      bullets <- c(
        "Invalid download reason ID.",
        i = "Use `show_all(reasons)` to see all valid reasons.",
        x = glue("{value} does not match an existing reason ID.")
      )
      abort(bullets, call = error_call)
    } else if(is.character(value) & !(value %in% show_all_reasons()$name)) {
      bullets <- c(
        "Invalid download reason name.",
        i = "Use `show_all(reasons)` to see all valid reasons.",
        x = glue("\"{value}\" does not match an existing reason name.")
      )
      abort(bullets, call = error_call)
    }
    if (is.character(value) & (value %in% show_all_reasons()$name)) {
      valid_reasons <- show_all_reasons()
      value_id <- valid_reasons |>
        filter(valid_reasons$name == value) |>
        select("id") |>
        pull("id")
      inform(c("v" = glue("Matched \"{value}\" to valid download reason ID {value_id}.")))
      value_id
    }else{
      value
    }
  }
}

#' catch all failure for unknown names
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
enforce_invalid_name <- function(name, error_call = caller_env()){
  bullets <- c(
    "Invalid option name.",
    i = "See `?galah_config()` for valid options.",
    x = glue("\"{name}\" is not a valid option name.")
  )
  abort(bullets, call = error_call)
}

#' Set behaviour for deriving correct atlas information
#' @importFrom rlang abort
#' @importFrom glue glue
#' @noRd
#' @keywords Internal
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

#' Provide a message if atlas is changed
#' @importFrom glue glue
#' @importFrom rlang inform
#' @noRd
#' @keywords Internal
check_atlas <- function(current_data, new_data){
  if(new_data$region != current_data$region){
    inform(glue(
      "Atlas selected: {new_data$organisation} ({new_data$acronym}) [{new_data$region}]"))
  }
  new_data
}