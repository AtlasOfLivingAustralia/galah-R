#' Internal function to `compute()` lists
#' Required for pagination
#' Should run a query with `max = 0` to get total n
#' Then use `max` and `offset` to paginate up to `n`
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
parse_lists <- function(.query){
  url <- url_parse(.query$url)
  n_requested <- as.integer(url$query$max)
  # make decisions about how much pagination is needed
  if(n_requested <= 500){ # we haven't hit pagination limit
    .query
  }else{ # more lists are requested
    n <- get_max_n(.query)
    n_pages <- ceiling(n$max_requested / n$paginate)
    offsets <- (seq_len(n_pages) - 1) * n$paginate
    result <- tibble(
      offset = offsets,
      max = c(
        rep(n$paginate, n_pages - 1),
        n$max_requested - offsets[n_pages]))
    result$url <- lapply(
      split(result, seq_len(nrow(result))),
      function(a){
        url$query <- list(offset = a$offset, max = a$max)
        url_build(url)
      }) |>
      unlist()
    .query$url <- select(result, "url")
  }
  .query
}

#' Internal function to retrieve max number of entries for an API
#' @noRd
#' @keywords Internal
get_max_n <- function(.query){
  url <- url_parse(.query$url)
  if(is_gbif()){
    count_field <- "count"
  }else{
    count_field <- "listCount"
  }
  n <- list(requested = as.integer(url$query$max), 
            paginate = 500, 
            max_available = {
              url$query <- list(max = 0)
              list(url = url_build(url),
                   headers = .query$headers) |>
                query_API() |>
                pluck(count_field) # NOTE: only tested for ALA                
            })
  n$max_requested <- min(c(n$requested, n$max_available))
  return(n)
}
