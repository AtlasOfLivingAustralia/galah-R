# @rdname show_all_minifunctions
# @export
show_all_apis <- function(){
  node_config
}

# @rdname search_minifunctions
# @export
search_apis <- function(query){
  df <- node_config
  df_string <- apply(
    df[, c("atlas", "system", "api_name", "called_by")], 
    1, 
    function(a){paste(a, collapse = " ")})
  df[grepl(tolower(query), tolower(df_string)), ]
}