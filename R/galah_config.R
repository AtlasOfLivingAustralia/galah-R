#' View or set package behaviour
#'
#' The `galah` package supports queries to a number of different data providers,
#' and once selected, it is desirable that all later queries are sent to that 
#' organisation. Rather than supply this information separately in each 
#' query, therefore, it is more parsimonious to cache that information centrally 
#' and call it as needed, which is what this function supports. Beyond choosing
#' an organisation, there are several other use cases for caching. Many
#' GBIF nodes require the user to supply a registered email address, 
#' password, and (in some cases) a reason for downloading data, all stored via
#' `galah_config()`. This function also provides a convenient place to control 
#' optional package behaviours, such as checking queries to ensure they are 
#' valid (`run_checks`), informing you via email when your downloads are ready 
#' (`send_email`), or controlling whether galah will provide updates on your
#' query as they are processed (`verbose`).
#' @param \dots Options can be defined using the form `name = "value"`, or 
#' as a (single) named list. See details for accepted fields.
#' @details
#' Valid arguments to this function are:
#' 
#'   *  `atlas` string: Living Atlas to point to, Australia by default. Can be 
#'   an organisation name, acronym, or region (see [show_all_atlases()] for 
#'   admissible values)
#'   * `authenticate` logical: should `galah` authenticate your queries using 
#'   JWT tokens? Defaults to `FALSE`.
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
#'  Defaults to `FALSE`.
#' @return Returns an object with classes `galah_config` and `list`, invisibly
#' if arguments are supplied.
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
#' 
#' # Optionally supply arguments via a named list
#' list(email = "your-email@email.com") |>
#'   galah_config()
#' }
#' @export
galah_config <- function(...) {
  
  # make sure dots are captured properly
  dots <- list(...)

  # set defaults, if this has not happened already
  if(length(potions::pour()) == 0) {
    potions::brew(default_config())
  }
  
  # add user-provided information
  if(length(dots) > 0){
    # check for deprecated `cache_directory`
    if(any(names(dots) == "cache_directory")){
      dots_location <- which(names(dots) == "cache_directory")
      value <- dots$cache_directory
      lifecycle::deprecate_warn(when = "2.0.0",
                                what = "galah_config(cache_directory)",
                                details = glue::glue("Use `galah_config(directory = \"{value}\")` instead.")
      )
      names(dots)[dots_location] <- "directory"
    }
    
    # add exception so that people can supply a named list to `galah_config()`
    # this avoids calling things like:
    # `x <- list(email = "something); do.call(galah_config, x)`
    if(length(dots) == 1){
      if(is.list(dots[[1]])){
        dots <- dots[[1]]
      }
    }
    
    # check all values in dots to ensure they are named
    if(length(dots) != length(names(dots))){
      c("All arguments to `galah_config() must be named.",
        i = "Did you use `==` instead of `=`?") |>
      cli::cli_abort()
    }
    
    # check all values in dots to ensure they are valid
    result <- restructure_config(dots)
    
    # look up what information has been given, write specific callouts for unusual cases
    supplied_names <- names(result)

    # add to `potions` object
    if(any(supplied_names == "atlas")){
      potions::brew(atlas = list(atlas = result$atlas))
      result <- result[names(result) != "atlas"]
    }
    
    if(length(result) > 0){
      potions::brew(result, method = "leaves")
    }
    
    # invisibly return
    x <- potions::pour()
    # check_authentication_argument(x)
    structure(x,
              class = c("galah_config", "list")) |>
      invisible()
  
  }else{
    # visibly return
    x <- potions::pour()
    # check_authentication_argument(x)
    structure(x,
              class = c("galah_config", "list"))
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
      authenticate = FALSE,
      directory = tempdir()),
    user = list(
      username = "",
      email = "",
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
  result <- purrr::map(names(dots),
                       \(a){validate_config(a, 
                                            dots[[a]],
                                            error_call = error_call)})
  names(result) <- names(dots)
  result
}

#' Catch errors in user-provided config
#' @noRd
#' @keywords Internal
validate_config <- function(name, 
                            value, 
                            error_call = rlang::caller_env()) {
  result <- switch(name,
         "atlas" = {
           value <- configure_atlas(value)
           # see whether atlases have changed, and if so, give a message
           check_atlas(potions::pour("atlas"), value)
         },
         "authenticate"    = enforce_logical(value),
         "caching"         = enforce_logical(value),
         "directory"       = check_directory(value),
         "download_reason_id" = enforce_download_reason(value),
         "email"           = enforce_character(value),
         "password"        = enforce_character(value),
         "run_checks"      = enforce_logical(value),
         "send_email"      = enforce_logical(value),
         "username"        = enforce_character(value),
         "verbose"         = enforce_logical(value),
         enforce_invalid_name(name,
                              error_call = error_call))
  result
}

#' Ensure some arguments are logical
#' @noRd
#' @keywords Internal
enforce_logical <- function(value, 
                            error_call = rlang::caller_env()){
  if (!is.logical(value)) {
    cli::cli_abort("Supplied value must be TRUE or FALSE.", 
                   call = error_call)
  }else{
    value
  }
}

#' Ensure a file exists
#' @noRd
#' @keywords Internal
enforce_exists <- function(value, 
                           error_call = rlang::caller_env()){
  if (!dir.exists(value)) {
    c("Cache directory does not exist.",
      i = "Does the directory entered exist?") |>
    cli::cli_abort(call = error_call)
  }else{
    value
  }
}

#' Ensure provided value is a string
#' @noRd
#' @keywords Internal
enforce_character <- function(value,
                              error_call = rlang::caller_env()){
  if (!is.character(value)) {
    c("Invalid type",
      i = "Value must be entered as a string.") |>
    cli::cli_abort(call = error_call)
  }else{
    value
  }
}

#' Ensure download reason is valid
#' @noRd
#' @keywords Internal
enforce_download_reason <- function(value, 
                                    error_call = rlang::caller_env()){
  # first ensure API is available. Currently missing for Brazil, for example.
  
  reasons_api_available <- url_lookup("metadata/reasons") |> 
    try(silent = TRUE)
  if(inherits(reasons_api_available, "try-error")){
    return(1)
  }else{
    if (is.numeric(value) & !(value %in% show_all_reasons()$id)) {
      c("Invalid download reason ID.",
        i = "Use `show_all(reasons)` to see all valid reasons.",
        x = "{value} does not match an existing reason ID.") |>
      cli::cli_abort(call = error_call)
    } else if(is.character(value) & !(value %in% show_all_reasons()$name)) {
      bullets <- c(
        "Invalid download reason name.",
        i = "Use `show_all(reasons)` to see all valid reasons.",
        x = "\"{value}\" does not match an existing reason name.") |>
      cli::cli_abort(call = error_call)
    }
    if (is.character(value) & (value %in% show_all_reasons()$name)) {
      valid_reasons <- show_all_reasons()
      value_id <- valid_reasons |>
        dplyr::filter(valid_reasons$name == value) |>
        dplyr::select("id") |>
        dplyr::pull("id")
      c("v" = "Matched \"{value}\" to valid download reason ID {value_id}.") |>
        cli::cli_inform(call = error_call)
      value_id
    }else{
      value
    }
  }
}

#' catch all failure for unknown names
#' @noRd
#' @keywords Internal
enforce_invalid_name <- function(name,
                                 error_call = rlang::caller_env()){
  c("Invalid option name.",
    i = "See `?galah_config()` for valid options.",
    x = "\"{name}\" is not a valid option name.") |>
  cli::cli_abort(call = error_call)
}

#' Set behaviour for deriving correct atlas information
#' @noRd
#' @keywords Internal
configure_atlas <- function(query,
                            error_call = rlang::caller_env()){
  
  comparison <- do.call(c, node_metadata)
  comparison <- comparison[!is.na(comparison)] |> 
    as.character()
  lookup <- utils::adist(query,
                         comparison,
                         ignore.case = TRUE)[1, ]
  
  if(all(lookup > 2)){
    c("Unsupported atlas provided.",
      i = "Use `show_all(atlases)` to see supported atlases.",
      x = "\"{query}\" is not a valid atlas.") |>
    cli::cli_abort(call = error_call)
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
#' @noRd
#' @keywords Internal
check_atlas <- function(current_data, new_data){
  if(new_data$region != current_data$region){
    cli::cli_inform(
      "Atlas selected: {new_data$organisation} ({new_data$acronym}) [{new_data$region}]")
  }
  new_data
}

#' if authentication is requested, cache config info
#' @noRd
#' @keywords Internal
check_authentication_argument <- function(x){
  # NOTE: This is the only place in galah where we _silently_ query
  # an API. For safety and clarity reasons, I've added the following steps:
  # 1. giving some notice to the user that this has been performed 
  # 2. adding a warning message if the API call fails
  if(isTRUE(purrr::pluck(x, "package", "authenticate")) & # value set to TRUE by user
     is.null(retrieve_cache("config")) # not already cached
  ){
    cli::cli_inform("Caching `config` information to support authentication")
    config <- request_metadata(type = "config") |>
      collect() |>
      try(silent = TRUE)
    if(inherits(config, "try-error")){
      c("`galah_config()` tried caching `config` information for authentication purposes, but failed.",
        i = "This could mean you are offline or that the API is unavailable.",
        i = "To try again, call `show_all_config()` or `galah_config(authenticate = TRUE)`") |>
        cli::cli_warn()
    }
  }
}
# NOTE: Need to add trigger to stop this process for orgs that are not ALA.
# This will mean moving it higher up in the workflow.