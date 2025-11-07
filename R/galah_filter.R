#' @rdname filter.data_request
#' @order 4
#' @export
galah_filter <- function(..., profile = NULL){
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object()
  check_named_input(dots)
  switch(class(dots[[1]])[1],
         "data_request" = {
           if(is_gbif()){
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
         if(is_gbif()){
           parse_quosures_data_gbif(dots)
         }else{
           parse_quosures_data(dots)  
         }
  )
}