#' Internal function to `collect()` taxa
#' @noRd
#' @keywords Internal
collect_taxa <- function(.query){
  if(grepl("namematching|name-matching", .query$url$url[1])){
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
collect_taxa_namematching <- function(.query){
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
  issues <- unlist(result$issues)
  
  if(length(issues) > 1) {
    issues_c <- glue::glue_collapse(issues, sep = ", ")
  } else {
    issues_c <- issues
  }
  
  # add issues to result
  result <- result |>   
    dplyr::mutate("search_term" = search_terms, 
                  .before = "success",
                  issues = issues_c)
  
  # Check for homonyms
  if(any(colnames(result) == "issues")){
    check_homonyms(result)
  }
  
  # Check for invalid search terms
  if (galah_config()$package$verbose) {
    check_search_terms(result)
  }
  
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> dplyr::select(dplyr::any_of(wanted_columns("taxa")))
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
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> dplyr::select(dplyr::any_of(wanted_columns("taxa")))
}

#' Internal function to `collect()` taxa for GBIF
#' @noRd
#' @keywords Internal
collect_taxa_gbif <- function(.query){
  search_terms <- .query$url$search_term
  result <- query_API(.query) |>
    clean_gbif_taxa() |>
    dplyr::bind_rows() |>
    dplyr::mutate("search_term" = search_terms, 
                  .before = 1)
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> 
    dplyr::select(dplyr::any_of(wanted_columns("taxa")))
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
      list_of_results <- list_of_results |> unlist()
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
    flat_lists_only() |>
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
      dplyr::rename("taxonConceptID" = "key")
  }
  
  result <- result |>
    dplyr::mutate("search_term" = search_terms, .before = "success")

  # Check for invalid search terms
  if (galah_config()$package$verbose) {
    check_search_terms(result)
  }
  
  names(result) <- rename_columns(names(result), 
                                  type = "taxa") # old code
  result <- result |> 
    dplyr::select(dplyr::any_of(wanted_columns("taxa")))
  attr(result, "call") <- "identifiers"
  attr(result, "region") <- potions::pour("atlas", "region") 
  result
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
      invalid_taxa_truncated <- c(invalid_taxa[1:3], glue("+ {n_invalid - 3} more"))
      list_invalid_taxa <- glue::glue_collapse(invalid_taxa_truncated, 
                                               sep = "\", \"", 
                                               last = "\" ")
      bullets <- c(
        bullets, 
        cli::cli_text(format_error_bullets(c("{.yellow \"{list_invalid_taxa}}")))
        )
    } else {
      list_invalid_taxa <- glue::glue_collapse(invalid_taxa, 
                                               sep = "\", \"")
      bullets <- c(
        bullets, 
        cli::cli_text(format_error_bullets(c("{.yellow \"{list_invalid_taxa}\"}")))
      )
    }
    
    cli::cli_inform(bullets)
    cli::cli_end(d)
  }
}

#' Internal function to check for homonyms in search term provided to 
#' `search_taxa()`
#' @noRd
#' @keywords Internal
check_homonyms <- function(result) {
  if ("homonym" %in% result$issues) {
  homonym_taxa <- result[result$issues %in% "homonym",]$search_term
  list_homonym_taxa <- glue::glue_collapse(homonym_taxa, 
                                           sep = ", ")
  bullets <- c(
    "Search returned multiple taxa due to a homonym issue.",
    i = "Please provide another rank in your search to clarify taxa.",
    i = "Use a `tibble` to clarify taxa, see `?search_taxa`.", 
    x = glue("Homonym issue with \"{list_homonym_taxa}\".")
  )
  cli::cli_warn(bullets)
  }
}
