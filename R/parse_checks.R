#' Internal function to run metadata checks
#' This is useful for testing, particularly in testing `galah_select()`
#' called by `collapse()`
#' @noRd
#' @keywords Internal
parse_checks <- function(.query){
  # "data/" functions require pre-processing of metadata,
  # as do `unnest()`/`show_values()` functions
  if(grepl("^data/", .query$type) |
     grepl("-unnest$", .query$type)
  ){
    # some checks should happen regardless of `run_checks`
    .query <- .query |>
      check_identifiers() |> 
      check_select()
    if(pour("package", "run_checks")) {
      .query <- .query |>
        check_login() |>
        check_reason() |>
        check_fields() |>
        check_profiles()
    }
    .query <- remove_metadata(.query)
  }
  .query
}

#' Internal function to reduce size of internally computed objects
#' called by `compute.query()`
#' @noRd
#' @keywords Internal
remove_metadata <- function(.query){
  names_lookup <- grepl("^metadata/", names(.query))
  if(any(names_lookup)){
    x <- .query[!names_lookup]
  }else{
    x <- .query
  }
  class(x) <- "query"
  x
}