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
  attr(result, "call") <- "assertions" # needed for `show_values()` to work
  attr(result, "region") <- pour("atlas", "region") # needed for caching to work
  result
}

#' Internal function to `collect()` collections
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_collections <- function(.data){
  result <- query_API(.data) |> bind_rows()
  attr(result, "call") <- "collections"
  attr(result, "region") <- pour("atlas", "region") 
  result
}

#' Internal function to `collect()` datasets
#' @importFrom dplyr bind_rows
#' @noRd
#' @keywords Internal
collect_datasets <- function(.data){
  result <- query_API(.data) |> bind_rows()
  attr(result, "call") <- "datasets"
  attr(result, "region") <- pour("atlas", "region") 
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
  if(!is.null(.data$url)){ # i.e. there is no cached `tibble`
    result <- query_API(.data) |>
      bind_rows() |>
      mutate(id = name) |>
      select(all_of(wanted_columns("fields"))) |>
      mutate(type = "fields") |>
      bind_rows(galah_internal_archived$media,
                galah_internal_archived$other)
    attr(result, "call") <- "fields"
    attr(result, "region") <- pour("atlas", "region")
    check_internal_cache(fields = result)
    result
  }else{ # this should only happen when `data` slot is present in place of `url`
    check_internal_cache()[["fields"]]
  }
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
  attr(result, "region") <- pour("atlas", "region") 
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
  attr(result, "region") <- pour("atlas", "region") 
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
  if(!is.null(.data$url)){
    result <- query_API(.data) |>
      bind_rows() |>
      filter(!duplicated(id)) |>
      arrange(id) |>
      select(all_of(wanted_columns(type = "profile")))
    attr(result, "call") <- "profiles"
    attr(result, "region") <- pour("atlas", "region") 
    check_internal_cache(show_all_profiles = df)
    result
  }else{
    check_internal_cache()[["profiles"]]
  }
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
  if(!is.null(.data$url)){
    result <- query_API(.data) |> 
      bind_rows() |>
      filter(!deprecated) |>
      select(all_of(wanted_columns("reasons"))) |>
      arrange(id)
    attr(result, "call") <- "reasons"
    attr(result, "region") <- pour("atlas", "region") 
    check_internal_cache(reasons = df)
    result
  }else{
    check_internal_cache()[["reasons"]]
  }
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
  attr(result, "region") <- pour("atlas", "region") 
  result
}