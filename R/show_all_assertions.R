#' @export show_all_assertions
#' @rdname show_all_minifunctions
show_all_assertions <- function(){
  get_assertions() |> as_tibble()
}

#' @rdname search_minifunctions
#' @export search_assertions
search_assertions <- function(query){
  df <- show_all_assertions()
  df[grepl(tolower(query), tolower(df$description)), ]
}


# internal function
get_assertions <- function() {
  url <- server_config("records_base_url")
  assertions <- atlas_GET(url, path = "assertions/codes")
  if(is.null(assertions)){
    NULL
  }else{
    assertions$data_type <- "logical"
    names(assertions) <- rename_columns(names(assertions), type = "assertions")
    assertions <- assertions[wanted_columns("assertions")]
    assertions$type <- "assertions"
    assertions
  }
}