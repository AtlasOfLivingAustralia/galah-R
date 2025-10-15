#' Internal function to run `as_query()` for type `data/distributions`
#' @noRd
#' @keywords Internal
as_query_distributions_data <- function(.query,
                                        error_call = rlang::caller_env()){
  identify_supplied <- !is.null(.query$identify)
  filter_supplied <- !is.null(.query$filter)
  if(identify_supplied & filter_supplied){
    cli::cli_abort("`collapse(type = 'distributions')` only accepts one of `filter()` or `identify()`, not both",
                   call = error_call)
  }
  if(identify_supplied){
    url <- url_lookup("data/distributions-taxa", 
                      lsid = "`TAXON_PLACEHOLDER`")
  }else if(filter_supplied){
    values <- strsplit(.query$filter$value, "\\|")[[1]]
    urls <- purrr::map(values, \(x){
      url_lookup("data/distributions-id", id = x)}) |>
      unlist()
    url <- tibble::tibble(url = urls)
  }else{ # i.e. neither supplied, get all distributions via ID column
    url <- url_lookup("data/distributions-id")
  }
  result <- list(type = "data/distributions",
                 url = url,
                 headers = build_headers())  
  class(result) <- "query"
  return(result)
}

#' Internal function to create a distributions query
#' @noRd
#' @keywords Internal
as_query_distributions_metadata <- function(.query){
  url <- url_lookup("metadata/distributions")
  result <- list(type = "metadata/distributions",
                 url = url,
                 headers = build_headers()) 
  class(result) <- "query"
  return(result)
}