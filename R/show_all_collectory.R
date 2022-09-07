#' @rdname show_all_minifunctions
#' @export
show_all_collections <- function(){
  url <- atlas_url("collections_collections")
  atlas_GET(url) |> tibble()
}

#' @rdname show_all_minifunctions
#' @export
show_all_datasets <- function(){
  url <- atlas_url("collections_datasets")
  atlas_GET(url) |> tibble()
}

#' @rdname show_all_minifunctions
#' @export
show_all_providers <- function(){
  url <- atlas_url("collections_providers")
  atlas_GET(url) |> tibble()
}

#' @rdname search_minifunctions
#' @export
search_collections <- function(query){
  df <- show_all_collections()
  df[grepl(tolower(query), tolower(df$name)), ]
}

#' @rdname search_minifunctions
#' @export
search_datasets <- function(query){
  df <- show_all_datasets()
  df[grepl(tolower(query), tolower(df$name)), ]
}

#' @rdname search_minifunctions
#' @export
search_providers <- function(query){
  df <- show_all_providers()
  df[grepl(tolower(query), tolower(df$name)), ]
}