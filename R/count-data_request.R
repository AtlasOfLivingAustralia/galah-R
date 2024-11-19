#' Count the observations in each group
#' 
#' `count()` lets you quickly count the unique values of one or more variables. 
#' It is evaluated lazily.
#' @name count.data_request
#' @param x An object of class `data_request`, created using [galah_call()]
#' @param wt currently ignored
#' @param ... currently ignored
#' @param sort currently ignored
#' @param name currently ignored
#' @importFrom dplyr count
#' @export
count.data_request <- function(x, 
                               ..., 
                               wt, 
                               sort, 
                               name){
  x$type <- switch(x$type, 
                   "occurrences" = "occurrences-count",
                   "species" = "species-count",
                   "media" = abort("type = 'media' is not supported by `count()`"),
                   abort("`count()` only supports `type = 'occurrences' or` `'species'`"))
  x
}