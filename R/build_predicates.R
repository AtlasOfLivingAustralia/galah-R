#' join all queries
#' NOTE: There is a maximum of 101k entries in total. Should be possible to enforce that here
#' @param an object of class `query_set`
#' @noRd
#' @keywords Internal
build_predicates <- function(x){
  x$filter |>
    join_predicates(parse_predicates_identify(x$identify)) |>
    join_predicates(parse_predicates_location(x$geolocate))

}

#' Internal function to join predicates together
#' @param a predicate
#' @param ... two or more predicates to join to x
#' @noRd 
#' @keywords Internal
join_predicates <- function(x, y = NULL){

  if(is.null(y)){
    x
  }else{
    # for `and` queries, we extract everything, add new content, then rebuild

    if(is_and_query(x)){
      x <- x$predicates
    }

    # add content together
    filters_list <- c(check_predicate_wrapping(x), 
                      check_predicate_wrapping(y))
    names(filters_list) <- NULL
    list(type = "and",
         predicates = filters_list)
  }
}

#' simple check for whether predicates begin with `and`
#' @noRd
#' @keywords Internal
is_and_query <- function(x){
  if(purrr::pluck_exists(x, "type")){
    if(purrr::pluck(x, "type") == "and"){
      TRUE
    }else{
      FALSE
    }
  }else{
    FALSE
  }
}

#' simple check to ensure lists are joined correctly
#' @noRd
#' @keywords Internal
check_predicate_wrapping <- function(x){
  if(length(x) > 2 &
     any(names(x) %in% c("in", "key", "value"))){
    list(x)
  }else{
    x
  }
}

#' handle taxonomic queries
#' @noRd
#' @keywords Internal
parse_predicates_identify <- function(x){
  if(!is.null(x)){
    result <- purrr::map(x$taxon_concept_id,
               \(a){list(type = "equals",
                         key = "TAXON_KEY",
                         value = a)})
    if(length(result) > 1){
      list(type = "or",
           result)
    }else{
      result
    }
  }else{
    NULL
  }
}

#' handle spatial queries
#' NOTE: There is a limit of 10k points in geometry; should be possible to enforce that here
#' @noRd
#' @keywords Internal
parse_predicates_location <- function(location){
  if(!is.null(location)) {
    # if location is for a point radius vs polygon/bbox
    if(!is.null(names(location))){
      if(all(!is.null(location$radius))) { # `galah_radius()` will always pass radius argument
        list(type = "geoDistance",
             latitude = location$lat,
             longitude = location$lon,
             distance = glue::glue("{location$radius} km")) |>
          list()
      }else{
        list(type = "within", 
             geometry = location) |>
          list()
      }
    }else{
      list(type = "within", 
           geometry = location) |>
        list()
    }
  }else{
    NULL
  }
}

#' handle `group_by` in predicates
#' @noRd
#' @keywords Internal
parse_predicates_groupby <- function(groupby){
  if(!is.null(groupby)){
    array(data = gbif_upper_case(groupby$name),
          dim = length(groupby$name),
          dimnames = NULL)
  }else{
    NULL
  }
}

#' clean up a list
#' @noRd
#' @keywords Internal
remove_nulls_from_list <- function(x){
  if(length(x) < 1){
    NULL
  }else{
    x[!unlist(purrr::map(x, is.null))]  
  }
}