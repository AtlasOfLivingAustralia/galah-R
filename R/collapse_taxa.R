#' Internal function to `collapse()` for `type = "taxa"`, `method = "metadata`
#' @noRd
#' @keywords Internal
collapse_taxa <- function(.query){
  if(is.null(.query$identify)){
    result <- list(type = "metadata/taxa")
    class(result) <- "query"
    result
  }else{
    if(ncol(.query$identify) > 1 | colnames(.query$identify)[1] != "search_term"){
      collapse_taxa_multiple(.query)
    }else{
      collapse_taxa_single(.query)
    }
  }
}

#' Internal function to `collapse()` for a single taxonomic name 
#' @noRd
#' @keywords Internal
collapse_taxa_single <- function(.query){
  urls <- lapply(.query$identify$search_term,
                 function(a){url_lookup("metadata/taxa-single",
                                        name = a)}) |>
    unlist()
  search_terms <- .query$identify$search_term
  # build object and return
  result <- list(type = "metadata/taxa-single",
                 url = tibble(url = urls, 
                              search_term = search_terms),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}

#' Internal function to `collapse()` where multiple taxonomic levels are given 
#' @noRd
#' @keywords Internal
collapse_taxa_multiple <- function(.query){
  split_list <- split(.query$identify, seq_len(nrow(.query$identify)))
  base_url <- url_lookup("metadata/taxa-multiple") |>
    url_parse()
  urls <- lapply(split_list,
                 function(a, base_url){
                   base_url$query <- as.list(a)
                   url_build(base_url)
                 },
                 base_url = base_url) |>
    unlist()
  search_terms <- lapply(split_list,
                         function(a){paste(a, collapse = "_")}) |>
    unlist()
  # build object and return
  result <- list(type = "metadata/taxa-multiple",
                 url = tibble(url = urls, 
                              search_term = search_terms),
                 headers = build_headers())
  class(result) <- "query"
  return(result)
}
