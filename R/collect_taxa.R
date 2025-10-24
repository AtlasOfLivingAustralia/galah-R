#' Internal function to `collect()` taxa
#' @noRd
#' @keywords Internal
collect_taxa <- function(.query){
  if(stringr::str_detect(.query$url$url[1], "namematching|name-matching")){
    collect_taxa_namematching(.query) # Australia, Spain, Sweden
  }else{
    if(is_gbif()){
      collect_taxa_gbif(.query)
    }else{
      collect_taxa_la(.query)  # tested for Austria, UK 
    }
  }
}

#' Internal function to `collect()` taxa for Atlas of Living Australia
#' @noRd
#' @keywords Internal
collect_taxa_namematching <- function(.query,
                                      error_call = rlang::caller_env()){
  search_terms <- .query$url$search_term
  result <- purrr::map(query_API(.query), 
                       build_tibble_from_nested_list) |> 
    dplyr::bind_rows()
  # break pipe for use case where all search terms are dubious (i.e. no taxonConceptID)
  # if(any(colnames(result) == "taxonConceptID")){
    # NOTE: This code was meant to remove duplicates, but also removes all rows with NAs (which we don't want)
    #       Might be worth returning to if this functionality is needed
    # result <- filter(result, !duplicated(taxonConceptID))
  # }
  # handle one or more returned issues values
  issues_vec <- purrr::map(result$issues,
                           \(a){
                             b <- a[[1]]
                             if(length(b) <= 1){
                               b
                             }else{
                               glue::glue_collapse(b, sep = ", ")
                             }
                           }) |>
    unlist()
  # add issues to result
  result <- result |>
    dplyr::select(-"issues") |>
    dplyr::mutate("search_term" = search_terms, 
                  .before = "success",
                  issues = issues_vec)
  # Check for homonyms
  check_homonyms(result, error_call = error_call)
  # Check for invalid search terms
  if (galah_config()$package$verbose) {
    check_search_terms(result)
  }
  result |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    parse_select(.query)
}

#' Internal function to `collect()` taxa for other living atlases
#' @noRd
#' @keywords Internal
collect_taxa_la <- function(.query){
  search_terms <- .query$url$search_term
  result <- query_API(.query) |>
    clean_la_taxa(search_terms = search_terms) |>
    dplyr::bind_rows()
  if(ncol(result) > 1){
    name <- switch(potions::pour("atlas", "region"),
                   "France" = "referenceID",
                   "Portugal" = "usageKey",
                   "guid")
    result <- result |>
      dplyr::filter(!duplicated({{name}})) |>
      dplyr::mutate("search_term" = search_terms)
  }
  result |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    parse_select(.query)
}

#' Internal function to `collect()` taxa for GBIF
#' @noRd
#' @keywords Internal
collect_taxa_gbif <- function(.query){
  search_terms <- .query$url$search_term
  query_API(.query) |>
    clean_gbif_taxa() |>
    dplyr::bind_rows() |>
    dplyr::mutate("search_term" = search_terms, 
                  .before = 1) |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    parse_select(.query)
}

#' Internal function to do cleaning for GBIF
#' @param result a list from a taxonomic web service
#' @noRd
#' @keywords Internal
clean_gbif_taxa <- function(result){
  purrr::map(result,
             \(a){
               c(
                 purrr::pluck(a$usage),
                 {
                   x <- purrr::map(a$classification,
                                   .f = \(b){b$name})
                   names(x) <- purrr::map(a$classification,
                                          .f = \(b){tolower(b$rank)})
                   x
                 },
                 a$diagnostics[lengths(a$diagnostics) < 2]
               )
             })
}

#' Internal function to do cleaning
#' @param result a list from a taxonomic web service
#' @noRd
#' @keywords Internal
clean_la_taxa <- function(result, search_terms){
  purrr::map(result, function(a){

    # capture results
    if("_embedded" %in% names(a)) { # e.g. France
      list_of_results <- a |>
        purrr::pluck("_embedded", "taxa")
    } else {
      if("searchResults" %in% names(a)) {
      list_of_results <- a |>
        purrr::pluck("searchResults", "results")
      } else { # e.g. Portugal (single result)
        list_of_results <- list(a)
      }
    }

    # find best string match to search term
    if (length(list_of_results) > 1) { # i.e. more than one match
      if ("name" %in% list_of_results[[1]]) {
        taxon_names <- purrr::map(list_of_results, 
                                  function(b){b$name}) |>
          unlist()
      } else { # e.g. France
        taxon_names <- purrr::map(list_of_results, 
                                  function(b){b$scientificName}) |>
          unlist()
      }
        string_distances <- utils::adist(
          tolower(search_terms), 
          tolower(taxon_names))[1, ]
        min_distance <- which(string_distances == min(string_distances))
        
        if (length(min_distance) > 1) { # i.e. 2+ identical entries
          json_length <- lengths(list_of_results)
          min_length <- which(string_distances == min(string_distances))[1] 
          # NOTE: [1] added above to ensure uniqueness
          # i.e. if multiple answers have the same amount of data, we pick the first
          list_of_results <- list_of_results[[min_length]]
        } else {
          list_of_results <- list_of_results[[min_distance]]
        }
    } else {
      list_of_results <- list_of_results
    }
    
    # unlist if necessary
    atlas <- potions::pour("atlas", "region")
    if (any(atlas %in% c("France", "Portugal"))) {
      list_of_results <- list_of_results |> 
        unlist()
    }
    list_of_results
  })
}

#' Internal function to `collect()` identifiers
#' @noRd
#' @keywords Internal
collect_identifiers <- function(.query){
  search_terms <- .query$url$search_term
  result <- query_API(.query) |>
    purrr::map(tidy_list_columns) |>
    dplyr::bind_rows()
  
  if(any(colnames(result) == "taxonConceptID")){
    result <- result |>
      dplyr::filter(!duplicated(result$taxonConceptID))
  }

  if(!any(colnames(result) == "success")){ # GBIF doesn't indicate success
    # we avoid `is_gbif()` here because other atlases use GBIF APIs
    result$success <- TRUE
    result <- result |>
      dplyr::relocate(success, .before = 1) |>
      parse_rename(.query)
  }
  
  result <- result |>
    dplyr::mutate("search_term" = search_terms, .before = "success")

  # Check for invalid search terms
  if (galah_config()$package$verbose) {
    check_search_terms(result)
  }
  
  result |>
    dplyr::rename_with(camel_to_snake_case) |>
    parse_rename(.query) |>
    parse_select(.query)
}

#' Internal function to check search terms provided to `search_taxa()`
#' @noRd
#' @keywords Internal
check_search_terms <- function(result, atlas) {
  if (!all(result$success)) {
    atlas <- potions::pour("atlas", "region")
    
    d <- cli::cli_div(theme = list(span.bold = list("font-weight" = "bold"),
                                   span.yellow = list(color = "yellow")))
    
    invalid_taxa <- result[!result$success,]$search_term
    n_invalid <- length(invalid_taxa)
    n_all <- nrow(result)
    n_valid <- n_all - n_invalid
    
    bullets <- c(
      cli::cli_text("Matched {.bold {n_valid} of {n_all}} taxonomic search terms in selected atlas ({atlas})."),
      "!" = cli::cli_text("{.yellow {n_invalid} unmatched search term{?s}:}")
    )
    if (n_invalid > 3) {
      invalid_taxa_truncated <- c(invalid_taxa[1:3], glue::glue("+ {n_invalid - 3} more"))
      list_invalid_taxa <- glue::glue_collapse(invalid_taxa_truncated, 
                                               sep = "\", \"", 
                                               last = "\" ")
    } else {
      list_invalid_taxa <- glue::glue_collapse(invalid_taxa, 
                                               sep = "\", \"")
    }
    bullets <- c(
      bullets,
      c("{.yellow \"{list_invalid_taxa}\"}") |>
        rlang::format_error_bullets() |>
        cli::cli_text())
    
    cli::cli_inform(bullets)
    cli::cli_end(d)
  }
}

#' Internal function to check for homonyms in search term provided to 
#' `search_taxa()`
#' @noRd
#' @keywords Internal
check_homonyms <- function(result,
                           error_call = rlang::caller_env()) {
  homonym_check <- grepl("homonym", result$issues)
  if(any(homonym_check)){
    homonym_taxa <- result |>
      dplyr::filter(homonym_check == TRUE) |>
      dplyr::pull("search_term")
    list_homonym_taxa <- glue::glue_collapse(homonym_taxa, 
                                             sep = ", ")
    c("Search returned multiple taxa due to a homonym issue.",
      i = "Please provide another rank in your search to clarify taxa.",
      i = "Use a `tibble` to clarify taxa, see `?search_taxa`.", 
      x = "Homonym issue with \"{list_homonym_taxa}\".") |>
      cli::cli_warn(call = error_call)
  }
}
