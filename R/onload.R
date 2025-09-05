#' Set-up for galah during loading
#' @noRd
#' @keywords Internal
.onLoad <- function(libname, pkgname) {
    if (pkgname == "galah") {
      
      # set up storage of standard information via {potions}
      potions::brew(.pkg = "galah")
      galah_config() # to cache defaults
      options(list("check_internal_cache" = galah_internal_cached))
      
      # get information to display to the user
      galah_version <-  "version unknown"
      suppressWarnings(
        try(galah_version <- utils::packageDescription("galah")[["Version"]],
            silent = TRUE)) ## get the galah version, if we can
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
        i = cli::col_magenta('galah is currently configured to query {current_node} ({current_url}).'),
        i = cli::col_magenta('You can see all supported organisations with `show_all(atlases)`.'),
        i = cli::col_magenta('To change organisations, use e.g. `galah_config(atlas = \"GBIF\")`.')
      ) |>
      cli::cli_inform(class = c("packageStartupMessage",  
                                "simpleMessage", 
                                "message", 
                                "condition"))
    }
}
