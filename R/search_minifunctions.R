#' @name search_minifunctions
#' @aliases search_atlases search_reasons search_taxa search_identifiers search_ranks search_fields search_values search_assertions search_profiles search_profile_attributes search_providers search_collections search_datasets
#' @title Functions for searching detailed metadata
#' @description Some descriptive text
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' Search using a single term
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa("Reptilia")
#' ```
#' 
#' Note that `search_taxa()` is not case sensitive
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa("reptilia") # not case sensitive
#' ```
#'
#' Search multiple taxa. `search_taxa()` will return one row per taxon
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_taxa(c("reptilia", "mammalia"))
#' ```
#' 
#' `galah_identify()` uses `search_taxa()` to narrow data queries
#' 
#' Look up a unique taxon identifier
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_identifiers(identifier = "https://id.biodiversity.org.au/node/apni/2914510")
#' ```
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() |>
#'   galah_identify("reptilia") |>
#'   atlas_counts()
#' ```
#' 
#' Search for all fields that use include the word "date"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("date")
#' ```
#' 
#' Search for all fields with the string "basisofrecord"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("basisofrecord")
#' ```
#' 
#' Search for all fields that have information for "marine"
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("marine") |> 
#'   head() # only show first 5 results
#' ```
#' 
#' Search for all Wordclim layers
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' search_fields("worldclim")
#' ```
#'

NULL