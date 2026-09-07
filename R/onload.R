#' Set up for galah when `loadNamespace` is called
#' @noRd
#' @keywords Internal
.onLoad <- function(libname, pkgname) {
  if (pkgname == "galah") {
    set_up_potions()
  }
}

#' Internal function to set up potions storage. 
#' Called by `.onLoad`, but also by `galah_config()` when `galah` not previously called by `library()` 
#' (Issue 298)
#' @noRd
#' @keywords Internal
set_up_potions <- function(){
  stored_options <- getOption("potions-pkg")
  if(is.null(stored_options)){
    # set up storage of standard information via {potions}
    potions::brew(default_config(), .pkg = "galah") # set up caching of behaviour
  }
}

#' Remove galah-specific information when `unloadNamespace` is called
#' @noRd
#' @keywords Internal
.onUnload <- function(libpath) {
  potions::drain_package("galah")
}

#' Set-up for galah when `library()` is called
#' @noRd
#' @keywords Internal
.onAttach <- function(libname, pkgname) {
  if (pkgname == "galah") {

    set_up_potions()

    # get information to display to the user
    ## get the galah version, if we can
    galah_version <-  "version unknown"
    suppressWarnings(
      try(galah_version <- utils::packageDescription("galah")[["Version"]],
          silent = TRUE))

    # show currently-selected atlas
    current_node <- potions::pour("atlas", .pkg = "galah") |>
      purrr::pluck("acronym")
    current_url <- show_all_atlases() |>
      dplyr::filter(.data$acronym == current_node) |>
      dplyr::pull("url") |>
      stringr::str_replace("^https://", "")

    # display a message
    # NOTE: This message *must* have the following classes to enable them
    # to be controlled programmatically.
    # see ?packageStartupMessage (required by `check()`)
    startup_message <- function() {
      lines <- cli::cli_fmt({
        cli::cli_text("galah version {galah_version}")
        cli::cli_bullets(c(
          "*" = cli::col_magenta("This package is currently configured to query {current_node} ({current_url})."),
          "i" = cli::col_magenta('Change this setting globally by using e.g. `galah_config(atlas = \"GBIF\")`.'),
          " " = cli::col_magenta('Or for a single query by opening your pipe with e.g. `galah_call(from = "Spain")`.'),
          "i" = cli::col_magenta('See {.strong all} supported organisations with `show_all(atlases)`.')
          ))
      })
      paste(lines, collapse = "\n")
    }
    
    startup_message() |>  
      packageStartupMessage()
  }
}
  