#' @rdname group_by.data_request
#' @export
galah_group_by <- function(...){
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  switch(class(dots[[1]])[1],
         "data_request" = {
           df <- parse_quosures_basic(dots[-1]) |>
             parse_group_by()
           update_request_object(dots[[1]],
                                 group_by = df)
         },
         {
           parse_quosures_basic(dots) |>
             parse_group_by()
         })
}