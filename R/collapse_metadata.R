#' Internal function to `collapse()` lists
#' Required for pagination
#' Should run a query with `max = 0` to get total n
#' Then use `max` and `offset` to paginate up to `n`
#' @noRd
#' @keywords Internal
collapse_lists <- function(.query){
  if(is.null(.query$url)){
    .query
  }else if(inherits(.query$url, "tbl_df")){
    .query
  }else{
    url <- httr2::url_parse(.query$url)
    n_requested <- as.integer(url$query$max)
    # make decisions about how much pagination is needed
    if(n_requested <= 500){ # we haven't hit pagination limit
      .query
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
    }
    .query
  }
}

#' Internal function to retrieve max number of entries for an API
#' @noRd
#' @keywords Internal
get_max_n <- function(.query){
  url <- httr2::url_parse(.query$url)
  if(is_gbif()){
    count_field <- "count"
  }else{
    count_field <- "listCount"
  }
  n <- list(requested = as.integer(url$query$max), 
            paginate = 500, 
            max_available = {
              url$query <- list(max = 0)
              list(url = httr2::url_build(url),
                   headers = .query$headers) |>
                query_API() |>
                purrr::pluck(count_field) # NOTE: only tested for ALA                
            })
  n$max_requested <- min(c(n$requested, n$max_available))
  return(n)
}


#' Internal function to call `collapse` for `request_metadata(type = "profiles-unnest")`
#' @noRd
#' @keywords Internal
collapse_profile_values <- function(.query,
                                    error_call){
  url <- .query |>
    purrr::pluck("url") |>
    httr2::url_parse()
  profile_name <- extract_profile_name(url)
  short_name <- profile_short_name(profile_name,
                                   error_call = error_call)
  if (!potions::pour("atlas", "region") == "Spain") {
    path_name <- url |>
      purrr::pluck("path") |>
      dirname()
    url$path <- glue::glue("{path_name}/{short_name}")
  }
  list(type = .query$type,
       url = httr2::url_build(url)) |>
    enforce_select_query(.query) |>
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
extract_profile_name <- function(url) {
  atlas <- potions::pour("atlas", "region")
  if (atlas == "Spain") {
    profile_name <- url |>
      purrr::pluck("query", "profileName")
  } else {
    profile_name <- url |>
      purrr::pluck("path") |>
      basename()
  }
  return(profile_name)
}
