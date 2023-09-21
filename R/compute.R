# NOTE: compute is where queries can be cached with the specified atlas;
# but also where checks are run to ensure later queries are valid

# if calling `compute()` after `request_data()` 
#' @rdname collect.data_request
#' @export
compute.data_request <- function(.data){
  .data$type <- check_type(.data$type)
  collapse(.data) |>
    switch_compute()
}

# if calling `compute()` after `collapse()`
#' @rdname collect.data_request
#' @export
compute.data_query <- function(.data){
  switch_compute(.data)
}

#' Internal function to determine which type of call to compute
#' @noRd
#' @keywords Internal
switch_compute <- function(.data){
  check_login(.data)
  .data <- .data |>
    check_identifiers() |>
    check_fields()
  switch(.data$type, 
         "doi" = compute_doi(.data),
         "media" = {class(.data) <- "data_response"; return(.data)},
         "occurrences" = {compute_occurrences(.data)},
         "occurrences-count" = compute_occurrences_count(.data),
         "species" = {class(.data) <- "data_response"; return(.data)},
         "species-count" = compute_species_count(.data),
         abort("unknown `type`"))   
}

# if calling `compute()` after `request_metadata()` 
#' @rdname collect.data_request
#' @export
compute.metadata_request <- function(.data){
  result <- collapse(.data)
  check_login(result)
  class(result) <- "metadata_response"
  return(result)
}

# if calling `compute()` after `collapse()` after `request_metadata()` 
#' @rdname collect.data_request
#' @export
compute.metadata_query <- function(.data){
  check_login(.data)
  class(.data) <- "metadata_response"
  return(.data)
}

# if calling `compute()` after `request_files()` 
#' @rdname collect.data_request
#' @export
compute.files_request <- function(.data){
  result <- collapse(.data)
  check_login(result)
  class(result) <- "files_response"
  return(result)
}

# if calling `compute()` after `collapse()` after `request_files()` 
#' @rdname collect.data_request
#' @export
compute.files_query <- function(.data){
  check_login(.data)
  class(.data) <- "files_response"
  return(.data)
}

# if calling `compute()` after `collapse()` after `request_values()` 
#' @rdname collect.data_request
#' @export
compute.values_query <- function(.data){
  switch(.data$type,
         "collections" = compute_basic_values(.data),
         "datasets" = compute_basic_values(.data),
         "fields" = compute_field_values(.data),
         "lists" = compute_basic_values(.data),
         "profiles" = compute_profile_values(.data),
         "providers" = compute_basic_values(.data),
         "taxa" = compute_basic_values(.data),
         abort("unrecognised 'type'"))
}

# if calling `compute()` after `request_values()` 
#' @rdname collect.data_request
#' @export
compute.values_request <- function(.data){
  result <- collapse(.data) |>
    compute()
}