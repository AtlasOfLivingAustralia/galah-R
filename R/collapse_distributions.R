#' Internal function to `collapse` for type `data/distributions`
#' @importFrom rlang abort
#' @noRd
#' @keywords Internal
collapse_distributions <- function(.query){
  identify_supplied <- !is.null(.query$identify)
  filter_supplied <- !is.null(.query$filter)
  if(identify_supplied & filter_supplied){
    abort("`collapse(type = 'distributions')` only accepts one of `filter()` or `identify()`, not both")
  }
  if(identify_supplied){
    url <- url_lookup("data/distributions-taxa", lsid = "`TAXON_PLACEHOLDER`")
  }else if(filter_supplied){
    values <- strsplit(.query$filter$value, "\\|")[[1]]
    urls <- map(values, \(x){
      url_lookup("data/distributions-id", id = x)}) |>
      unlist()
    url <- tibble(url = urls)
  }else{ # i.e. neither supplied, get all distributions via ID column
    url <- url_lookup("data/distributions-id")
  }
  result <- list(type = "data/distributions",
                 url = url,
                 headers = build_headers())  
  class(result) <- "query"
  return(result)
}