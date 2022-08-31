# this code works; but the lists themselves are very poorly curated,
# meaning there is no benefit to providing these at present.

# @rdname show_all_minifunctions
# @export
show_all_lists <- function(){
  url <- server_config("lists_base_url")
  do.call(rbind, list(
    atlas_GET(url, path = "speciesList?max=1000&offset=0")$lists,
    atlas_GET(url, path = "speciesList?max=1000&offset=1000")$lists,
    atlas_GET(url, path = "speciesList?max=1000&offset=2000")$lists
  )) |> 
    tibble()

}

# @rdname search_minifunctions
# @export
search_lists <- function(query){
  df <- show_all_lists()
  df[grepl(tolower(query), tolower(df$name)), ]
}