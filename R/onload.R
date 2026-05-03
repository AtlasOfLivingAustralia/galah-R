#' Set up for galah when `loadNamespace` is called
#' @noRd
#' @keywords Internal
.onLoad <- function(libname, pkgname) {
  if (pkgname == "galah") {
    # set up storage of standard information via {potions}
    potions::brew(.pkg = "galah") # set up caching of behaviour
    quiet_config <- purrr::quietly(galah_config)
    config_info <- quiet_config() # to cache defaults without raising a message
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
    c(
      glue::glue("galah version {galah_version}"),
      "\n", # note: glue wipes newlines, so have to be outside
      cli::col_magenta(glue::glue('This package is currently configured to query {current_node} ({current_url}).\n')),
      "\n",
      i = cli::col_magenta('- You can change this setting globally using using e.g. `galah_config(atlas = \"GBIF\")`.'),
      "\n",
      i = cli::col_magenta('- Or for a single query by opening your pipe with e.g. `request_data(from = "Spain")`.'),
      "\n",
      i = cli::col_magenta('- To see *all* supported organisations, run `show_all(atlases)`.')
    ) |>
      packageStartupMessage()
  }
}
  