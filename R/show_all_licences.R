#' @rdname show_all_minifunctions
#' @export
show_all_licences <- function(){
  result <- atlas_url("image_licences") |> 
    atlas_GET() |> 
    tibble()
  result[, c("id", "name", "acronym", "url")] 
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