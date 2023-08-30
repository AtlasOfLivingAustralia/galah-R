#' Internal function to `collect()` assertions
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_assertions <- function(.data){
  # get data
  result <- query_API(.data) |>
    bind_rows()
  names(result) <- rename_columns(names(result), type = "assertions")
  result <- result[wanted_columns("assertions")]
  result$type <- "assertions"
  # slice if requested
  # NOTE: this is old code. 
  # could easily use slice(), or even better, pass `max` to API
  if(!is.null(.data$limit)){
    limit_rows <- seq_len(min(nrow(result), .data$limit))
    assertions <- result[limit_rows, ]
  }
  result
}

#' Internal function to `collect()` collections
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_collections <- function(.data){
  query_API(.data) |> bind_rows()
}

#' Internal function to `collect()` datasets
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_datasets <- function(.data){
  query_API(.data) |> bind_rows()
}

#' Internal function to `collect()` fields
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_fields <- function(.data){
  query_API(.data) |>
    bind_rows() |>
    mutate(id = name) |>
    select(all_of(wanted_columns("fields"))) |>
    mutate(type = "fields")
}

#' Internal function to `collect()` licences
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_licences <- function(.data){
  query_API(.data) |> 
    bind_rows() |>
    select(all_of(c("id", "name", "acronym", "url"))) |> 
    arrange(id)
}

#' Internal function to `collect()` lists
#' @importFrom dplyr bind_rows
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_lists <- function(.data){
  query_API(.data) |>
    pluck("lists") |>
    bind_rows()
}

#' Internal function to `collect()` profiles
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_profiles <- function(.data){
  x <- query_API(.data) |>
    bind_rows()
  x |>
    filter(!duplicated(id)) |>
    arrange(id) |>
    select(all_of(wanted_columns(type = "profile")))
  # check_internal_cache(show_all_profiles = df)
}

#' Internal function to `collect()` providers
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_providers <- function(.data){
  query_API(.data) |> 
    bind_rows()
}

#' Internal function to `collect()` reasons
#' NOTE: decision whether to draw data from cache is made by `collapse()`, 
#' so doesn't need to be handled again here.
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_reasons <- function(.data){
  query_API(.data) |> 
    bind_rows() |>
    filter(!deprecated) |>
    select(all_of(wanted_columns("reasons"))) |>
    arrange(id)
  # check_internal_cache(show_all_reasons = df)
}

#' Internal function to `collect()` identifiers
#' @importFrom dplyr any_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_identifiers <- function(.data){
  search_terms <- .data$url$search_term
  result <- query_API(.data) |>
    bind_rows() |>
    filter(!duplicated(taxonConceptID)) |>
    mutate("search_term" = search_terms, .before = "success")
  names(result) <- rename_columns(names(result), type = "taxa") # old code
  result |> select(any_of(wanted_columns("taxa")))
}