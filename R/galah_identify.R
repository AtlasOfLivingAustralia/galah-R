#' Narrow a query by passing taxonomic identifiers
#'
#' When conducting a search or creating a data query, it is common to identify 
#' a known taxon or group of taxa to narrow down the records or results returned. 
#' This function allows users to pass scientific names or taxonomic identifiers
#' with pipes to provide data only for the biological group of interest.
#'
#' @param ... one or more scientific names (if `search = TRUE`) or taxonomic 
#'   identifiers (if `search = FALSE`); or an object of class `ala_id` (from
#'   `search_taxa`), `gbifid`, or `nbnid` (from `taxize`) for 
#'   international atlases.
#' @param search (logical); should the results in question be passed to
#'   `search_taxa`? Ignored if an object of class `ala_id`, `gbifid`, or `nbnid` 
#'   is given to `...`.
#'
#' @seealso [search_taxa()] to find identifiers from scientific names;
#' [search_identifiers()] for how to get names if taxonomic identifiers 
#' are already known.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' `galah_identify()` is used to identify taxa you want returned in a search or 
#' a data query. It is good to use [search_taxa()] and [search_identifiers()] 
#' first to check that the taxa you provide to `galah_identify()` return the 
#' correct results.
#' 
#' Specify a taxon. A valid taxon will return an identifier.
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_identify("reptilia")
#' ```
#' 
#' Specify more than one taxon at a time.
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_identify("reptilia", "mammalia", "aves", "pisces")
#' ```
#' 
#' Use `galah_identify()` to narrow your queries
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() %>% 
#'   galah_identify("Eolophus") %>%
#'   atlas_counts()
#' ```
#' 
#' If you already know a valid taxon identifier, add it and set `search = FALSE`.
#'
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() %>% 
#'   galah_identify("urn:lsid:biodiversity.org.au:afd.taxon:b2de5e40-df8f-4339-827d-25e63454a4a2", 
#'                  search = FALSE) %>%
#'   atlas_counts()
#' ```
#' 
#' 
#' @export
galah_identify <- function(..., search = TRUE) {

  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")
  checked_dots <- detect_data_request(dots)
  if (!inherits(checked_dots, "quosures")) {
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  } else {
    is_data_request <- FALSE
  }

  if (length(dots) > 0) {

    # basic checking
    check_queries(dots) # capture named inputs
    input_query <- parse_basic_quosures(dots) # convert dots to query

    # get cached behaviour
    atlas <- getOption("galah_config")$atlas
    run_checks <- getOption("galah_config")$run_checks
    verbose <- getOption("galah_config")$verbose

    # check for types first
    if (inherits(input_query, "ala_id")) {
      query <- input_query$taxon_concept_id
    } else if (inherits(input_query, c("gbifid+", "nbnid+"))) { # from taxize
      query <- as.character(input_query)
    } else { # if the input isn't of known type, try to find IDs
      if (search) {
        check_atlas(atlas)
        lookup <- search_taxa(input_query)
        if (is.null(lookup$taxon_concept_id)) {
          bullets <- c(
            "`galah_identify` didn't return anything.",
            i = "Did you use `search_taxa` to check whether your search species the correct taxa?"
          )
          abort(bullets, call = caller_env())
        } else {
          query <- lookup$taxon_concept_id[!is.na(lookup$taxon_concept_id)]
          if (verbose) {
            n_provided <- length(input_query)
            n_returned <- length(query)
            check_number_returned(n_provided, n_returned)
          }
        }
      } else { # i.e. user has passed search = FALSE
        if (atlas == "Australia" && run_checks) {
          lookup <- search_identifiers(input_query)
          if (is.null(lookup$taxon_concept_id)) {
            bullets <- c(
              "`galah_identify` didn't return anything.",
              i = "Did you use `search_identifiers` to check whether your search species the correct taxa?"
            )
            abort(bullets, call = caller_env())
          } else {
            query <- lookup$taxon_concept_id[!is.na(lookup$taxon_concept_id)]
            n_provided <- length(input_query)
            n_returned <- length(query)
            check_number_returned(n_provided, n_returned)
          }
        } else {
          query <- input_query # pass unchanged
        }
      } # end for search == FALSE
    } # end for unknown types
    
    check_is_character(query)
    result <- tibble(identifier = query)
    
  } else { # if empty, return correct class, but no values
    result <- as_tibble(data.frame(identifier = character()))
  }

  # if a data request was supplied, return one
  class(result) <- append(class(result), "galah_identify")
  if (is_data_request) {
    update_galah_call(data_request, identify = result)
  } else {
    result
  }
}


# checker function based on `galah_filter.R/check_filters`
check_queries <- function(dots, error_call = caller_env()) {
  if(any(have_name(dots))){
    bullets <- c(
      "We detected a named input.",
      i = glue("This usually means that you've used `=` somewhere"),
      i = glue("`galah_identity` doesn't require equations")
    )
    abort(bullets, call = error_call)
  }
}


check_number_returned <- function(n_in, n_out, error_call = caller_env()) {
  if(n_out < n_in){
    warn(
      glue(
        
        "Unmatched taxa. Results returned for {n_out} of {n_in} taxon IDs")
    )
  }
}

check_atlas <- function(atlas, error_call = caller_env()) {
  if(atlas != "Australia"){
    atlas_origin <- switch(atlas,
      "UK" = "UK",
      "Sweden" = "Swedish",
      "Austria" = "Austrian",
      "Guatemala" = "Guatemalan",
      "Spain" = "Spanish"
    )
    bullets <- c(glue("Searching is not supported for the {atlas_origin} atlas."),
      i = "try using the `taxize` package to search instead",
      i = "taxonomic identifiers can be passed to `galah_identify` by setting `search = FALSE`"
    )
    abort(bullets, call = error_call)
  }
}


# it is possible that the above will lead to non-character 
# arguments being passed (if search = FALSE and run_checks = FALSE)
# check this
check_is_character <- function(query, error_call = caller_env()){
  if(!inherits(query, "character")){
    lookup <- search_taxa(query)
    query <- lookup$taxon_concept_id[!is.na(lookup$taxon_concept_id)]
    
    bullets <- c(
      "We didn't recognise the object passed to `galah_identify` isn't from a recognised class.",
      i = "Recognised classes are `ala_id`, `gbifid`, `nbnid` or `character`",
      i = "Use `search_taxa` to lookup taxon information."
    )
    abort(bullets, call = error_call)
  }
}