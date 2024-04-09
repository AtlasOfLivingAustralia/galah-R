#' Internal function to `collect()` taxa
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @noRd
#' @keywords Internal
collect_taxa <- function(.query){
  if(grepl("namematching", .query$url$url[1])){
    collect_taxa_namematching(.query) # Australia, Sweden
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
  result <- lapply(query_API(.query), 
                   build_tibble_from_nested_list) |> 
    bind_rows()
  # break chain for use case where all search terms are dubious (i.e. no taxonConceptID)
  # if(any(colnames(result) == "taxonConceptID")){
    # NOTE: This code was meant to remove duplicates, but also removes all rows with NAs (which we don't want)
    #       Might be worth returning to if this functionality is needed
    # result <- filter(result, !duplicated(taxonConceptID))
  # }
  
  result <- result |>   
    mutate("search_term" = search_terms, .before = "success",
           issues = unlist(result$issues))
  
  # Check for homonyms
  if(any(colnames(result) == "issues")){
    check_homonyms(result)
  }
  
  # Check for invalid search terms
  if (galah_config()$package$verbose) {
    check_search_terms(result)
  }
  
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
}

#' Internal function to `collect()` taxa for other living atlases
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_taxa_la <- function(.query){
  search_terms <- .query$url$search_term
  result <- query_API(.query) |>
    clean_la_taxa(search_terms = search_terms) |>
    bind_rows()
  if(ncol(result) > 1){
    name <- switch(pour("atlas", "region"),
                   "France" = "referenceID",
                   "Portugal" = "usageKey",
                   "guid")
    result <- result |>
      filter(!duplicated({{name}})) |>
      mutate("search_term" = search_terms)
  }
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
}

#' Internal function to `collect()` taxa for GBIF
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_taxa_gbif <- function(.query){
  search_terms <- .query$url$search_term
  result <- query_API(.query) |>
    bind_rows() |>
    mutate("search_term" = search_terms, .before = 1)
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
}

#' Internal function to do cleaning
#' @param result a list from a taxonomic web service
#' @importFrom purrr pluck
#' @importFrom utils adist
#' @noRd
#' @keywords Internal
clean_la_taxa <- function(result, search_terms){
  lapply(result, function(a){

    # capture results
    if("_embedded" %in% names(a)) { # e.g. France
      list_of_results <- a |>
        pluck("_embedded", "taxa")
    } else {
      if("searchResults" %in% names(a)) {
      list_of_results <- a |>
        pluck("searchResults", "results")
      } else { # e.g. Portugal (single result)
        list_of_results <- list(a)
      }
    }

    # find best string match to search term
    if (length(list_of_results) > 1) { # i.e. more than one match
      if ("name" %in% list_of_results[[1]]) {
        taxon_names <- unlist(lapply(list_of_results, function(b){b$name}))
      } else { # e.g. France
        taxon_names <- unlist(lapply(list_of_results, function(b){b$scientificName}))
      }
        string_distances <- adist(
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
    atlas <- pour("atlas", "region")
    if (any(atlas %in% c("France", "Portugal"))) {
      list_of_results <- list_of_results |> unlist()
    }
    list_of_results
  })
}

#' Internal function to `collect()` identifiers
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_identifiers <- function(.query){
  search_terms <- .query$url$search_term
  result <- query_API(.query) |>
    flat_lists_only() |>
    bind_rows()
  if(any(colnames(result) == "taxonConceptID")){
    result <- result |>
      filter(!duplicated(result$taxonConceptID))
  }
  result <- result |>
    mutate("search_term" = search_terms, .before = "success")

  # Check for invalid search terms
  if (galah_config()$package$verbose) {
    check_search_terms(result)
  }
  
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
  attr(result, "call") <- "identifiers"
  attr(result, "region") <- pour("atlas", "region") 
  result
}

#' Internal function to check search terms provided to `search_taxa()`
#' @importFrom glue glue_collapse
#' @importFrom cli cli_div
#' @importFrom cli cli_text
#' @importFrom cli cli_end
#' @noRd
#' @keywords Internal
check_search_terms <- function(result, atlas) {
  if (!all(result$success)) {
    atlas <- pour("atlas", "region")
    
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
    
    inform(bullets)
    cli::cli_end(d)
  }
}

#' Internal function to check for homonyms in search term provided to 
#' `search_taxa()`
#' @importFrom glue glue_collapse
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
  warn(bullets)
  }
}
