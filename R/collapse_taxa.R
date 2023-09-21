#' Internal function to `collapse()` taxa
#' @noRd
#' @keywords Internal

# NOTE: make collapse_taxa() a wrapper function
  # collapse_taxa_single() calls the name-matching single api
  # collapse_taxa_multiple() calls the name-matching multiple api
collapse_taxa <- function(.data){
  if(is.null(.data$identify)){
    urls <- url_lookup("names_search_single")
    search_terms <- "no-name-supplied"
  }else{
    # case for searching by classification
    if(ncol(.data$identify) > 1 | !all(colnames(.data$identify) == "search_term")){
      split_list <- split(.data$identify, seq_len(nrow(.data$identify)))
      base_url <- url_parse(url_lookup("names_search_multiple"))
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
    }else{ # case for searching with one or more single strings
      urls <- lapply(.data$identify$search_term,
                     function(a){url_lookup("names_search_single", 
                                            name = a)}) |>
        unlist()
      search_terms <- .data$identify$search_term   
    }
  }
  # build object and return
  result <- list(type = .data$type,
                 url = tibble(url = urls, search_term = search_terms),
                 headers = build_headers())
  class(result) <- "metadata_query"
  return(result)
}