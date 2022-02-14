#' Show collections
#'
#' Show collections
#' @export
show_all_collections <- function(){
  url <- server_config("collections_base_url")
  atlas_GET(url, path = "collection") |> tibble()
}

#' Show Datasets
#'
#' Show Datasets
#' @export
show_all_datasets <- function(){
  url <- server_config("collections_base_url")
  atlas_GET(url, path = "dataResource") |> tibble()
}

#' Show providers
#'
#' Show providers
#' @export
show_all_providers <- function(){
  url <- server_config("collections_base_url")
  atlas_GET(url, path = "dataProvider") |> tibble()
}

#' Search collections
#'
#' Search collections
#' @export
search_collections <- function(query){
  df <- show_all_collections()
  df[grepl(tolower(query), tolower(df$name)), ]
}

#' Search datasets
#'
#' Search datasets
#' @export
search_datasets <- function(query){
  df <- show_all_datasets()
  df[grepl(tolower(query), tolower(df$name)), ]
}

#' Search providers
#'
#' Search providers
#' @export
search_providers <- function(query){
  df <- show_all_providers()
  df[grepl(tolower(query), tolower(df$name)), ]
}