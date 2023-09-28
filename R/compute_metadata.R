#' Internal function to `compute()` collections
#' @noRd
#' @keywords Internal
compute_collections <- function(.data){
  if(is_gbif()){
    # when someone searches for a specific collection using `filter()`
    # this is capped at n = 20 records
    if(grepl("suggest", .data$url)){
      result <- query_API(.data) |>
        bind_rows()
      if(nrow(result) > 1){
        .data$url <- tibble(
          url = paste(url_lookup("metadata/collections"), result$key, sep = "/"))
        .data
      }else{
        .data$url <- NULL
        .data$headers <- NULL
        .data
      }
    # i.e. no filter given
    }else{
      # this should, ideally, paginate as for `compute_lists()`
      # for now, leave as is
      .data
    }
  }else{
    .data
  }
}

#' Internal function to `compute()` lists
#' Required for pagination
#' Should run a query with `max = 0` to get total n
#' Then use `max` and `offset` to paginate up to `n`
#' @importFrom dplyr select
#' @noRd
#' @keywords Internal
compute_lists <- function(.data){
  url <- url_parse(.data$url)
  n <- get_max_n(.data)
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

#' Internal function to retrieve max number of entries for an API
#' @noRd
#' @keywords Internal
get_max_n <- function(.data){
  url <- url_parse(.data$url)
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
                   headers = .data$headers) |>
                query_API() |>
                pluck(count_field) # NOTE: only tested for ALA                
            })
  n$max_requested <- min(c(n$requested, n$max_available))
  return(n)
}