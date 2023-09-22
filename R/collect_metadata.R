#' Internal function to `collect()` assertions
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_assertions <- function(.data){
  # get data
  result <- lapply(query_API(.data), 
                   function(a){a[names(a) != "termsRequiredToTest"]}) |>
    bind_rows()
  names(result) <- rename_columns(names(result), type = "assertions")
  result <- result[wanted_columns("assertions")]
  result$type <- "assertions"
  attr(result, "call") <- "assertions" 
  # note: `attributes` are required for `show_values()` to work
  result
}

#' Internal function to `collect()` collections
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_collections <- function(.data){
  result <- query_API(.data) |> bind_rows()
  attr(result, "call") <- "collections"
  result
}

#' Internal function to `collect()` datasets
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_datasets <- function(.data){
  result <- query_API(.data) |> bind_rows()
  attr(result, "call") <- "datasets"
  result
}

#' Internal function to `collect()` fields
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_fields <- function(.data){
  result <- query_API(.data) |>
    bind_rows() |>
    mutate(id = name) |>
    select(all_of(wanted_columns("fields"))) |>
    mutate(type = "fields")
  attr(result, "call") <- "fields"
  result
}

#' Internal function to `collect()` licences
#' @importFrom dplyr all_of
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
collect_licences <- function(.data){
  result <- query_API(.data) |> 
    bind_rows() |>
    select(all_of(c("id", "name", "acronym", "url"))) |> 
    arrange(id)
  attr(result, "call") <- "licences"
  result
}

#' Internal function to `collect()` lists
#' @importFrom dplyr bind_rows
#' @importFrom purrr pluck
#' @noRd
#' @keywords Internal
collect_lists <- function(.data){
  result <- query_API(.data) |>
    pluck("lists") |>
    bind_rows()
  attr(result, "call") <- "lists"
  result
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
  result <- query_API(.data) |>
    bind_rows() |>
    filter(!duplicated(id)) |>
    arrange(id) |>
    select(all_of(wanted_columns(type = "profile")))
  attr(result, "call") <- "profiles"
  result
  # check_internal_cache(show_all_profiles = df)
}

#' Internal function to `collect()` providers
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_providers <- function(.data){
  result <- query_API(.data) |> 
    bind_rows()
  attr(result, "call") <- "providers"
  result
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
  result <- query_API(.data) |> 
    bind_rows() |>
    filter(!deprecated) |>
    select(all_of(wanted_columns("reasons"))) |>
    arrange(id)
  attr(result, "call") <- "reasons"
  result
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
  attr(result, "call") <- "identifiers"
  result
}