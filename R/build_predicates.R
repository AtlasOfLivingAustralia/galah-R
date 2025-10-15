#' join all queries
#' NOTE: There is a maximum of 101k entries in total. Should be possible to enforce that here
#' @noRd
#' @keywords Internal
build_predicates <- function(x){

  # combine provided information
  filters_list <- c(
    parse_predicates_filter(x),
    parse_predicates_identify(x$identify),
    parse_predicates_location(x$geolocate)) |>
    remove_nulls_from_list()

  # return correctly structured object
  if(length(filters_list) < 1){
    NULL
  }else{
    names(filters_list) <- NULL # important for parsing with toJSON
    list(type = "and", 
         predicates = filters_list)
    # NOTE: This is messy for length-1, but does work
  }
}

#' Cleanly handle filter args
#' @noRd
#' @keywords Internal
parse_predicates_filter <- function(x){
  if(is.null(x)){
    NULL
  }else{
    if(is_and_query(x)){
      x$filter$predicates
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