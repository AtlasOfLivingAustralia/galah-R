#' Internal function to `collect()` taxa
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @noRd
#' @keywords Internal
collect_taxa <- function(.data){
  result <- switch(pour("atlas", "region"),
                   "United Kingdom" = collect_taxa_uk(.data),
                   collect_taxa_la(.data))
}

#' Internal function to `collect()` taxa for living atlases
#' @noRd
#' @keywords Internal
collect_taxa_la <- function(.data){
  search_terms <- .data$url$search_term
  result <- query_API(.data) |>
    bind_rows() |>
    filter(!duplicated(taxonConceptID)) |>
    mutate("search_term" = search_terms, .before = "success")
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
}

#' Internal function to `collect()` taxa for UK
#' @noRd
#' @keywords Internal
collect_taxa_uk <- function(.data){
  search_terms <- .data$url$search_term
  result <- query_API(.data)
  result <- lapply(result, function(a){
    x <- pluck(a, !!!list("searchResults", "results"))
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
  }) |>
    bind_rows() |>
    filter(!duplicated(guid)) |>
    mutate("search_term" = search_terms, .before = "id")
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
}