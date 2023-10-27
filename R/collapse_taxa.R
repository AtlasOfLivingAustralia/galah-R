#' Internal function to `collapse()` for `type = "taxa"`, `method = "metadata`
#' @noRd
#' @keywords Internal
collapse_taxa <- function(q_obj){
  if(is.null(q_obj$identify)){
    result <- list(type = "metadata/taxa")
    class(result) <- "query"
    result
  }else{
    if(ncol(q_obj$identify) > 1 | colnames(q_obj$identify)[1] != "search_term"){
      collapse_taxa_multiple(q_obj)
    }else{
      collapse_taxa_single(q_obj)
    }
  }
}

#' Internal function to `collapse()` for a single taxonomic name 
#' @noRd
#' @keywords Internal
collapse_taxa_single <- function(q_obj){
  urls <- lapply(q_obj$identify$search_term,
                 function(a){url_lookup("metadata/taxa-single",
                                        name = a)}) |>
    unlist()
  search_terms <- q_obj$identify$search_term
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
collapse_taxa_multiple <- function(q_obj){
  split_list <- split(q_obj$identify, seq_len(nrow(q_obj$identify)))
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
