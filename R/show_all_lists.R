# this code works; but the lists themselves are very poorly curated,
# meaning there is no benefit to providing these at present.

# @rdname show_all_minifunctions
# @export
show_all_lists <- function(){
  url <- atlas_url("lists_all") |> 
    paste0(c(
      "?max=1000&offset=0", 
      "?max=1000&offset=1000", 
      "?max=1000&offset=2000")) 
  do.call(rbind, 
    lapply(url, function(a){atlas_GET(a)$lists})) |> 
    tibble()
}

# @rdname search_minifunctions
# @export
search_lists <- function(query){
  df <- show_all_lists()
  df[grepl(tolower(query), tolower(df$name)), ]
}