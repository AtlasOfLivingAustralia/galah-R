#' Internal function to `collapse()` lists
#' Required for pagination
#' Should run a query with `max = 0` to get total n
#' Then use `max` and `offset` to paginate up to `n`
#' @noRd
#' @keywords Internal
collapse_lists <- function(.query){
  if(is.null(.query$url)){
    as_query(.query)
  }else if(inherits(.query$url, "tbl_df")){
    as_query(.query)
  # in non-Australian atlases, ending in a digit means that pageSize or similar is set already
  }else if(stringr::str_detect(.query$url, "[:digit:]+$") & .query$atlas != "Australia"){
    as_query(.query)
  # set up 'new' lists code (May 2026)
  # rationale here is that we run a query to get the number of levels
  }else if(stringr::str_detect(.query$url, "[:digit:]+$") & .query$atlas == "Australia"){
    # first handle case where levels are pre-specified
    if(!is.null(.query$request$slice)){
      n_lists <- .query$request$slice$slice_n
    # otherwise calculate it
    }else{
      n_lists <- .query |> 
        query_API() |>
        purrr::pluck("listCount")
    }
    # calculate urls
    if(is.null(n_lists)){
      as_query(.query)
    }else{
      n_pages <- ceiling(n_lists * 0.001) # (i.e. 1/1000) Note that 1000 is the maximum
      base_url <- stringr::str_remove(.query$url, "pageSize=1$")
      if(n_pages < 2){
        .query$url <- tibble::tibble(url = glue::glue("{base_url}pageSize={n_lists}"))
      }else{
        .query$url <- tibble::tibble(url = glue::glue("{base_url}page={seq_len(n_pages)}&pageSize=1000"))
      }      
      as_query(.query)
    }
  # below is legacy, probably still important/used, but hard to be sure
  }else{
    url <- httr2::url_parse(.query$url)
    n_requested <- as.integer(url$query$max)
    # make decisions about how much pagination is needed
    if(n_requested <= 500){ # we haven't hit pagination limit
      as_query(.query)
    }else{ # more lists are requested
      n <- get_max_n(.query)
      n_pages <- ceiling(n$max_requested / n$paginate)
      offsets <- (seq_len(n_pages) - 1) * n$paginate
      result <- tibble::tibble(
        offset = offsets,
        max = c(
          rep(n$paginate, n_pages - 1),
          n$max_requested - offsets[n_pages]))
      result$url <- purrr::map(
        split(result, seq_len(nrow(result))),
        function(a){
          url$query <- list(offset = a$offset, max = a$max)
          httr2::url_build(url)
        }) |>
        unlist()
      .query$url <- dplyr::select(result, "url")
      as_query(.query)
    }
  }
}

#' Internal function to retrieve max number of entries for an API
#' @noRd
#' @keywords Internal
get_max_n <- function(.query){
  url <- httr2::url_parse(.query$url)
  if(.query$atlas == "Global"){
    count_field <- "count"
  }else{
    count_field <- "listCount"
  }
  n <- list(requested = as.integer(url$query$max), 
            paginate = 500, 
            max_available = {
              url$query <- list(max = 0)
              list(type = "metadata/list-count",
                   url = httr2::url_build(url),
                   headers = .query$headers) |>
                query_API() |>
                purrr::pluck(count_field) # NOTE: only tested for ALA                
            })
  n$max_requested <- min(c(n$requested, n$max_available))
  n
}

#' Internal function to ensure that list values are paginated properly
#' @noRd
#' @keywords Internal
collapse_lists_unnest <- function(.query, error_call){
  # get row length
  n_rows <- .query$`metadata/lists`$row_count[1]
  # if >30000, paginate
  if(n_rows > 30000){
    n_pages <- ceiling(row_count * (1/30000))
    # add additional urls to reach required number of pages to return all items
    initial_url <- .query$url
    url_tibble <- tibble::tibble(url = glue::glue("{initial_url}&page={seq_len(n_pages)}"))
    .query$url <- url_tibble
  }
  # return cleaned object
  as_query(.query[names(.query) != "metadata/lists"])  
}

#' Internal function to call `collapse` for `request_metadata(type = "profiles-unnest")`
#' @noRd
#' @keywords Internal
collapse_profile_values <- function(.query,
                                    error_call){
  url <- .query |>
    purrr::pluck("url") |>
    httr2::url_parse()
  profile_name <- extract_profile_name(.query, url)
  short_name <- profile_short_name(profile_name,
                                   error_call = error_call)
  if (.query$atlas != "Spain") {
    path_name <- url |>
      purrr::pluck("path") |>
      dirname()
    url$path <- glue::glue("{path_name}/{short_name}")
  }
  list(type = .query$type,
       atlas = .query$atlas,
       url = httr2::url_build(url)) |>
    as_query()
}
# this doesn't print for some reason

#' Internal function to convert between long and short names
#' for data profiles. Only used by `collapse_profile_values()`
#' @noRd
#' @keywords Internal
profile_short_name <- function(profile,
                               error_call) {
  valid_profiles <- show_all_profiles()
  short_name <- NA
  if (suppressWarnings(!is.na(as.numeric(profile)))) {
    # assume a profile id has been provided
    short_name <- valid_profiles[match(as.numeric(profile),
                                       valid_profiles$id),]$short_name
  } else {
    # try to match a short name or a long name
    if (profile %in% valid_profiles$name) {
      short_name <- valid_profiles[match(profile,
                                         valid_profiles$name), ]$short_name
    } else {
      if (profile %in% valid_profiles$short_name) {
        short_name <- profile
      }
    }
  }
  if (is.na(short_name)) {
    c(
      "Unknown profile detected.",
      i = "See a listing of valid data quality profiles with `show_all_profiles()`.") |>
    cli::cli_abort(call = error_call)
  }else{
    short_name
  }
}

#' Internal function to extract profile name from url
#' for data profiles. Only used by `compute_profile_values()`
#' @noRd
#' @keywords Internal
extract_profile_name <- function(.query, url) {
  if (.query$atlas == "Spain") {
    profile_name <- url |>
      purrr::pluck("query", "profileName")
  } else {
    profile_name <- url |>
      purrr::pluck("path") |>
      basename()
  }
  return(profile_name)
}
