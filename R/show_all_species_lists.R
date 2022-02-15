# this code works; but the lists themselves are very poorly curated,
# meaning there is no benefit to providing these at present.

# @rdname show_all_minifunctions
# @export
show_all_species_lists <- function(){
  url <- server_config("lists_base_url")
  atlas_GET(url, path = "speciesList")$lists |> tibble()
}

# @rdname search_minifunctions
# @export
search_species_lists <- function(query){
  df <- show_all_species_lists()
  df[grepl(tolower(query), tolower(df$name)), ]
}