#' Internal function to `collect()` taxa
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @noRd
#' @keywords Internal
collect_taxa <- function(.data){
  switch(pour("atlas", "region"),
         "Australia" = collect_taxa_australia(.data),
         collect_taxa_la(.data)) # tested for Austria, UK
}

#' Internal function to `collect()` taxa for Atlas of Living Australia
#' @noRd
#' @keywords Internal
collect_taxa_australia <- function(.data){
  search_terms <- .data$url$search_term
  result <- lapply(query_API(.data), 
                   build_tibble_from_nested_list) |> 
    bind_rows()
  # break chain for use case where all search terms are dubious (i.e. no taxonConceptID)
  if(any(colnames(result) == "taxonConceptID")){
    result <- filter(result, !duplicated(taxonConceptID))
  }
  result <- result |>   
    mutate("search_term" = search_terms, .before = "success")
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
}

#' Internal function to `collect()` taxa for other living atlases
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_taxa_la <- function(.data){
  search_terms <- .data$url$search_term
  result <- query_API(.data) |>
    clean_la_taxa(search_terms = search_terms) |>
    bind_rows() |>
    filter(!duplicated(guid)) |>
    mutate("search_term" = search_terms, .before = "id")
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
    x <- a |>
      pluck("searchResults", "results")
    if(length(x) > 1){ # i.e. more than one match
      taxon_names <- unlist(lapply(x, function(b){b$name}))
      string_distances <- adist(
        tolower(search_terms), 
        tolower(taxon_names))[1, ]
      min_distance <- which(string_distances == min(string_distances))
      if(length(min_distance) > 1){ # i.e. 2+ identical entries
        json_length <- lengths(x)
        min_length <- which(string_distances == min(string_distances))[1] 
        # NOTE: [1] added above to ensure uniqueness
        # i.e. if multiple answers have the same amount of data, we pick the first
        x[[min_length]]
      }else{
        x[[min_distance]]
      }
    }else{
      x
    }
  })
}