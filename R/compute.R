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
  check_fields(.data)
  switch(.data$type, 
         "occurrences-count" = compute_counts(.data),
         "species-count" = compute_counts(.data),
         "species" = {class(.data) <- "data_response"; return(.data)},
         "occurrences" = {compute_occurrences(.data)},
         "media" = {compute_media(.data)})   
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