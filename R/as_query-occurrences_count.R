#' collapse for type = "occurrences-count"
#' @keywords Internal
#' @param .query an object of class `data_request`
#' @noRd
as_query_occurrences_count <- function(.query){
  # NOTE: This is quite weird syntax; consider revising
  if(is_gbif()){
    function_name <- "as_query_occurrences_count_gbif"
    arg_names <- names(formals(as_query_occurrences_count_gbif))
  }else{
    function_name <- "as_query_occurrences_count_atlas"
    arg_names <- names(formals(as_query_occurrences_count_atlas))
  }
  custom_call <- .query[names(.query) %in% arg_names]
  class(custom_call) <- "data_request"
  do.call(function_name, custom_call)
}

#' collapse for counts on LAs
#' @keywords Internal
#' @noRd
as_query_occurrences_count_atlas <- function(identify = NULL, 
                                             filter = NULL, 
                                             geolocate = NULL,
                                             data_profile = NULL,
                                             group_by = NULL, 
                                             slice = NULL,
                                             arrange = NULL
){
  query <- build_query(identify, 
                       filter, 
                       geolocate, 
                       data_profile = data_profile) 
  # set behaviour depending on `group_by()`
  if(is.null(group_by)){
    url <- url_lookup("data/occurrences-count") |> 
      httr2::url_parse()
    url$query <- c(query, pageSize = 0)
    result <- list(type = "data/occurrences-count",
                   url = httr2::url_build(url),
                   headers = build_headers(),
                   filter = filter,
                   slot_name = "totalRecords")
  }else{
    url <- url_lookup("data/occurrences-count-groupby") |> 
      httr2::url_parse()
    facets <- as.list(group_by$name)
    names(facets) <- rep("facets", length(facets))
    if(is.null(slice)){
      # limits to 10,000 rows
      # TODO: This should ultimately be set by `slice` or `atlas_counts(limit = )`, not internally.
      #       Will need updating to avoid hidden limit setting here & in `compute_occurrences_count()`
      slice <- tibble::tibble(slice_n = 1e4, slice_called = FALSE) 
    }
    if(is.null(arrange)){
      arrange <- tibble::tibble(variable = "count", 
                                direction = "descending")
    }
    slice_arrange <- dplyr::bind_cols(slice, arrange) 
    arrange_list <- check_slice_arrange(slice_arrange)
    url$query <- c(query, facets, arrange_list)
    result <- list(type = "data/occurrences-count-groupby",
                   url = httr2::url_build(url),
                   headers = build_headers(),
                   filter = filter,
                   arrange = slice_arrange)
  }
  class(result) <- "query"
  return(result)
}

#' collapse for counts on GBIF
#' @importFrom httr2 url_build
#' @importFrom httr2 url_parse
#' @keywords Internal
#' @noRd
as_query_occurrences_count_gbif <- function(identify = NULL, 
                                            filter = NULL,
                                            geolocate = NULL,
                                            group_by = NULL,
                                            slice = NULL
                                            ){
  # compile supplied arguments into a list
  # honestly this is a little messy, but the alternative is to call 
  # [build_predicates()], which is messier as taxonomic info hasn't yet been 
  # parsed. Instead we call [build_predicates()] during [collapse_query()].
  predicates_info <- list(identify = identify, 
                          filter = filter, 
                          geolocate = geolocate,
                          group_by = group_by,
                          slice = ifelse(is.null(slice),
                                         tibble::tibble(slice_n = 30, slice_called = FALSE),
                                         slice), 
                          limit = 0)
  
  # get strings
  username <- potions::pour("user", "username", .pkg = "galah")
  password <- potions::pour("user", "password", .pkg = "galah")
  user_string <- glue::glue("{username}:{password}")
  
  # handle type
  if(is.null(group_by)){
    data_type <- "data/occurrences-count"
  }else{
    data_type <- "data/occurrences-count-groupby"
  }
  
  # build object
  ## Note that unlike with other atlases, parsing of `group_by` is handled
  ## by `collapse()` rather than here.
  result <- list(
    type = data_type,
    url = url_lookup("data/occurrences-count"),
    headers =  list(
      `User-Agent` = galah_version_string(), 
      `X-USER-AGENT` = galah_version_string(),
      `Content-Type` = "application/json",
      Accept = "application/json"),
    options = list(
      httpauth = 1,
      userpwd = user_string),
    body = predicates_info,
    slot_name = "count")
 
  # classify and return
  class(result) <- "query"
  result
}

#' Internal function to check `slice` and `arrange` for counts
#' @keywords Internal
#' @noRd
check_slice_arrange <- function(df){
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
