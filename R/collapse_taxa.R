#' Internal function to `collapse()` for `type = "taxa"`, `method = "metadata`
#' @noRd
#' @keywords Internal
collapse_taxa <- function(.data){
  if(is.null(.data$identify)){
    abort("No name supplied to `identify()`")
  }else{
    if(ncol(.data$identify) > 1 | colnames(.data$identify) != "search_term"){
      collapse_taxa_multiple(.data)
    }else{
      collapse_taxa_single(.data)
    }
  }
}

#' Internal function to `collapse()` for a single taxonomic name 
#' @noRd
#' @keywords Internal
collapse_taxa_single <- function(.data){
  urls <- lapply(.data$identify$search_term,
                 function(a){url_lookup("metadata/taxa-single",
                                        name = a)}) |>
    unlist()
  search_terms <- .data$identify$search_term
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
collapse_taxa_multiple <- function(.data){
  split_list <- split(.data$identify, seq_len(nrow(.data$identify)))
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