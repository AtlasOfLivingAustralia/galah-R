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
                                             apply_profile = NULL,
                                             group_by = NULL,
                                             distinct = NULL,
                                             slice_arrange = NULL
){
  query <- build_query(identify, 
                       filter, 
                       geolocate, 
                       apply_profile = apply_profile) 
  # set behaviour depending on `group_by()`
  if(is.null(group_by) & is.null(distinct)){
    url <- url_lookup("data/occurrences-count") |> 
      httr2::url_parse()
    url$query <- c(query, pageSize = 0)
    result <- list(type = "data/occurrences-count",
                   url = httr2::url_build(url),
                   headers = build_headers())
  }else{
    url <- url_lookup("data/occurrences-count-groupby") |> 
      httr2::url_parse()
    if(!is.null(group_by)){
      facets <- group_by$name
    }else{
      facets <- distinct$name
    }
    names(facets) <- rep("facets", length(facets))
    url$query <- c(query, facets, parse_slice_arrange(slice_arrange))
    result <- list(type = "data/occurrences-count-groupby",
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

#' collapse for counts on GBIF
#' @keywords Internal
#' @noRd
as_query_occurrences_count_gbif <- function(identify = NULL, 
                                            filter = NULL,
                                            geolocate = NULL,
                                            group_by = NULL,
                                            slice = NULL # probably broken
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
  list(type = data_type,
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
       slot_name = "count") |>
    as_prequery()
}
