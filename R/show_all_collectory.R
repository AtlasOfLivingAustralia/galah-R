#' @rdname show_all_minifunctions
#' @export
show_all_collections <- function(){
  url <- server_config("collections_base_url")
  atlas_GET(url, path = "collection") |> tibble()
}

#' @rdname show_all_minifunctions
#' @export
show_all_datasets <- function(){
  url <- server_config("collections_base_url")
  atlas_GET(url, path = "dataResource") |> tibble()
}

#' @rdname show_all_minifunctions
#' @export
show_all_providers <- function(){
  url <- server_config("collections_base_url")
  atlas_GET(url, path = "dataProvider") |> tibble()
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