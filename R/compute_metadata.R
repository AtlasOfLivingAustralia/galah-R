#' Internal function to `compute()` lists
#' Required for pagination
#' Should run a query with `max = 0` to get total n
#' Then use `max` and `offset` to paginate up to `n`
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
compute_lists <- function(.data){
  # lookup max number of lists
  url <- url_parse(.data$url)
  n <- list(requested = as.integer(url$query$max), 
            paginate = 500, 
            max_available = {
              url$query <- list(max = 0)
              list(url = url_build(url),
                   headers = .data$headers) |>
              query_API() |>
              pluck("listCount") # NOTE: only tested for ALA                
            })
  n$max_requested <- min(c(n$requested, n$max_available))
  # make decisions about how much pagination is needed
  if(n$max_requested <= n$paginate){ # we haven't hit pagination limit
    url$query$max <- n$max_requested
    .data$url <- url_build(url)
  }else{ # more lists are requested
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
    .data$url <- select(result, "url")
  }
  .data
}