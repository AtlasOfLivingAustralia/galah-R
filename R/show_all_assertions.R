#' @export show_all_assertions
#' @rdname show_all_minifunctions
show_all_assertions <- function(){
  url <- atlas_url("records_assertions") 
  assertions <- atlas_GET(url)
  if(is.null(assertions)){
    tibble()
  }else{
    assertions$data_type <- "logical"
    names(assertions) <- rename_columns(names(assertions), type = "assertions")
    assertions <- assertions[wanted_columns("assertions")]
    assertions$type <- "assertions"
    as_tibble(assertions)
  }
}

#' @rdname search_minifunctions
#' @export search_assertions
search_assertions <- function(query){
  df <- show_all_assertions()
  df[grepl(tolower(query), tolower(df$description)), ]
}