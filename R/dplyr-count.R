#' Count the observations in each group
#' 
#' `count()` lets you quickly count the unique values of one or more variables. 
#' It is evaluated lazily. `add_count()` is an equivalent that uses `mutate()`
#' to add a new column with group-wise counts.
#' @name count.data_request
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param wt currently ignored
#' @param ... currently ignored
#' @param sort currently ignored
#' @param name currently ignored
#' @export
count.data_request <- function(x, 
                               ..., 
                               wt, 
                               sort, 
                               name){
  count_switch(x) |>
    group_by(...)
}

#' Internal function called by `count.data_request()` and `distinct.data_request()`
#' @noRd
#' @keywords Internal
count_switch <- function(x){ 
  x$type <- switch(x$type, 
                   "occurrences" = "occurrences-count",
                   "occurrences-count" = "occurrences-count",
                   "species" = "species-count",
                   "species-count" = "species-count",
                   "media" = cli::cli_abort("type = 'media' is not supported by `count()`"),
                   cli::cli_abort("`count()` only supports `type = 'occurrences' or` `'species'`"))
  x
}

#' @rdname count.data_request
#' @export
add_count.data_request <- function(x,
                                  ...,
                                   wt = NULL,
                                   sort = FALSE,
                                   name = NULL){
  update_request_object(x, 
                        add_count = TRUE)
}
