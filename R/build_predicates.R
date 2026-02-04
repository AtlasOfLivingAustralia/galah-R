#' join all queries
#' NOTE: There is a maximum of 101k entries in total. Should be possible to enforce that here
#' @noRd
#' @keywords Internal
build_predicates <- function(x){
  
  # handle newly supplied information
  x_identify <- parse_predicates_identify(x$identify)
  x_location <- parse_predicates_location(x$geolocate)

  # for and queries, we extract everything, add new content, then rebuild
  if(is_and_query(x)){
    filters_list <- c(x$filter$predicates, x_identify, x_location) |>
      remove_nulls_from_list()
    names(filters_list) <- NULL
    list(type = "and",
         predicates = filters_list)
  }else{
    # if we have been given further information, use AND
    if(!is.null(x_identify) | !is.null(x_location)){
      filters_list <- c(x$filter, x_identify, x_location) |> # note: not x$filter$predicates
        remove_nulls_from_list()
      names(filters_list) <- NULL
      list(type = "and",
           predicates = filters_list)
    # otherwise we can pass what we were given (usually an OR statement)
    }else{
      x$filter
    }
  }
}

#' simple check for whether predicates begin with `and`
#' @noRd
#' @keywords Internal
is_and_query <- function(x){
  if(purrr::pluck_exists(x, "filter", "type")){
    if(purrr::pluck(x, "filter", "type") == "and"){
      TRUE
    }else{
      FALSE
    }
  }else{
    FALSE
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