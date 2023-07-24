# if calling `compute()` after `request_data()` 
#' @rdname collect.data_request
#' @export
compute.data_request <- function(.data){
  .data$type <- check_type(.data$type)
  collapse(.data) |>
    switch_compute()
}

# if calling `compute()` after `request_metadata()` 
# @rdname collect.data_request
# @export
# compute.metadata_request <- function(.data){
#   meta_collapse <- collapse(.data)
#   if(any(c("lists", "collections", "datasets", "providers") == .data$type)){
#     browser()
#     x <- collect(meta_collapse)
#     # NOT sure what to add here
#   }else{
#     class(.data) <- "metadata_response"
#     return(.data)
#   }
# }

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
  switch(.data$type, 
         "occurrences-count" = compute_counts(.data),
         "species-count" = compute_counts(.data),
         "doi" = abort(c(
           "`compute()` does not exist for `type = 'doi'`",
           i = "try `collect() instead")),
         "species" = abort(c(
           "`compute()` does not exist for `type = 'species'`",
           i = "try `collect() instead")),
         "occurrences" = {
           check_login(.data)
           compute_occurrences(.data)},
         "media" = {
           check_login(.data)
           compute_media(.data)})   
}