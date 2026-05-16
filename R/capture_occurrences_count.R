#' capture() for type = "occurrences-count"
#' @keywords Internal
#' @param .query an object of class `data_request`
#' @noRd
capture_occurrences_count <- function(.query){
  switch(.query$atlas,
         "Global" = capture_occurrences_count_gbif(.query),
         capture_occurrences_count_atlas(.query))
  }

#' capture() for counts on LAs
#' @keywords Internal
#' @noRd
capture_occurrences_count_atlas <- function(.query){
  query <- build_query(identify = .query$identify, 
                       filter = .query$filter, 
                       geolocate = .query$geolocate, 
                       apply_profile = .query$apply_profile,
                       atlas = .query$atlas) 
  # set behaviour depending on `group_by()`
  if(is.null(.query$group_by) & is.null(.query$distinct)){
    .query$type <- "data/occurrences-count"
    url <- url_lookup(.query) |> 
      httr2::url_parse()
    url$query <- c(query, pageSize = 0)
    result <- list(type = .query$type,
                   atlas = .query$atlas,
                   url = httr2::url_build(url),
                   headers = build_headers())
  }else{
    .query$type <- "data/occurrences-count-groupby"
    url <- url_lookup(.query) |> 
      httr2::url_parse()
    if(!is.null(.query$group_by)){
      facets <- .query$group_by$name
    }else{
      facets <- .query$distinct$name
    }
    names(facets) <- rep("facets", length(facets))
    url$query <- c(query, facets, parse_slice_arrange(.query$slice_arrange))
    result <- list(type = .query$type,
                   atlas = .query$atlas,
                   url = httr2::url_build(url),
                   headers = build_headers())
  }
  as_prequery(result)
}

#' Internal function to parse `slice` and `arrange` for counts
#' @keywords Internal
#' @noRd
parse_slice_arrange <- function(df){
  if(df$variable == "count"){ # arranged in descending order by default
    if(df$direction == "ascending"){
      list(fsort = "count", flimit = 0)
    }else{
      list(fsort = "count", flimit = df$slice_n)
    }
  }else{ # non-count fields are arranged in ascending order by default
    if(df$direction == "ascending"){
      list(fsort = "index", flimit = df$slice_n)
    }else{
      list(fsort = "index", flimit = 0)
    }
  }
}

#' capture() for counts on GBIF
#' @keywords Internal
#' @noRd
capture_occurrences_count_gbif <- function(.query){
  # compile supplied arguments into a list
  # honestly this is a little messy, but the alternative is to call 
  # [build_predicates()], which is messier as taxonomic info hasn't yet been 
  # parsed. Instead we call [build_predicates()] during [collapse_query()].
  predicates_info <- list(identify = .query$identify, 
                          filter = .query$filter, 
                          geolocate = .query$geolocate,
                          group_by = .query$group_by,
                          slice = ifelse(is.null(.query$slice),
                                         tibble::tibble(slice_n = 30, slice_called = FALSE),
                                         .query$slice), 
                          limit = 0)
  
  # get strings
  username <- .query$authenticate$username
  password <- .query$authenticate$password
  user_string <- glue::glue("{username}:{password}")
  
  # handle type
  if(is.null(.query$group_by)){
    .query$type <- "data/occurrences-count"
  }else{
    .query$type <- "data/occurrences-count-groupby"
  }
  
  # build object
  ## Note that unlike with other atlases, parsing of `group_by` is handled
  ## by `collapse()` rather than here.
  list(type = .query$type,
       atlas = .query$atlas,
       url = url_lookup(.query),
       headers =  list(
         `User-Agent` = galah_version_string(), 
         `X-USER-AGENT` = galah_version_string(),
         `Content-Type` = "application/json",
         Accept = "application/json"),
       options = list(
         httpauth = 1,
         userpwd = user_string),
       body = predicates_info) |>
    as_prequery()
}
