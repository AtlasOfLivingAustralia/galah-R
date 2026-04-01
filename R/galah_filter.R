#' @rdname superseded_functions
#' @order 2
#' @export
galah_filter <- function(...){
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  check_named_input(dots)
  switch(class(dots[[1]])[1],
         "data_request" = {
           if(dots[[1]]$atlas == "Global"){
             filters <- parse_quosures_data_gbif(dots[-1])  # `handle_quosures_GBIF.R`
           }else{
             filters <- parse_quosures_data(dots[-1]) # `handle_quosures.R`
           }
           update_request_object(dots[[1]],
                                 filter = filters)
         },
         "metadata_request" = {
           parse_quosures_metadata(dots[[1]], dots[-1])
         },
         "files_request" = {
           input <- dots[[1]]
           parsed_dots <- parse_quosures_files(dots[-1])
           input$filter <- parsed_dots$data
           input$type <- parsed_dots$variable
           input
         },
         # NOTE: below here is triggered if user calls `galah_filter()` without
         # `galah_call()`/`request_data()` first. In this case we only have
         # global settings to go on.
         if(potions::pour("atlas", "region", .pkg = "galah") == "Global"){
           parse_quosures_data_gbif(dots)
         }else{
           parse_quosures_data(dots)  
         }
  )
}