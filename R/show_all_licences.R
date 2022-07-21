#' @rdname show_all_minifunctions
#' @export
show_all_licences <- function(){
  url <- server_config("images_base_url")
  result <- atlas_GET(url, path = "licence")
  result[, c("id", "name", "acronym", "url")] |> tibble()
}

#' @rdname search_minifunctions
#' @export
search_licences <- function(query){
  df <- show_all_licences()
  df[grepl(
    tolower(query), 
    tolower(
      apply(df[, c("name", "acronym")], 
      1, 
      function(a){paste(a, collapse = " ")})
    )
  ), ]
}