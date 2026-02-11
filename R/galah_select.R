#' @rdname select.data_request
#' @export
galah_select <- function(..., group = NULL){
  dots <- rlang::enquos(..., .ignore_empty = "all") |>
    detect_request_object() |>
    as.list()
  if(is_gbif()){
    cli::cli_text("`select()` is not supported for GBIF occurrence downloads API v1: skipping")
    if(inherits(dots[[1]], "data_request")){
      dots[[1]]
    }else{
      NULL
    }
  }else{
    if(length(dots) < 1){
      list(quosure = c(), summary = "") |>
        add_group(group)
    }
    else if(inherits(dots[[1]], "data_request")){
      list(quosure = dots[-1],
           summary = generate_summary(dots[-1])) |>
        add_group(group) |>
      update_request_object(dots[[1]],
                            select = _) 
    }else{
      list(quosure = dots,
           summary = generate_summary(dots)) |>
        add_group(group)
    } 
  }
}