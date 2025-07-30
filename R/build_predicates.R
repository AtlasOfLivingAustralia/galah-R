#' Build predicates
#' 
#' predicates are JSON scripts for passing to GBIF offline downloads API.
#' https://www.gbif.org/developer/occurrence
#' params x A list with slots relevant to building predicates
#' @noRd
#' @keywords Internal
build_predicates <- function(
  x
  # format = "SIMPLE_CSV" # i.e. default is to return occurrences
){
  list(
    creator = jsonlite::unbox(
      potions::pour("user", "username", .pkg = "galah")),
    notificationAddresses = jsonlite::unbox(
      potions::pour("user", "email", .pkg = "galah")),
    sendNotification = jsonlite::unbox(
      potions::pour("package", "send_email", .pkg = "galah")),
    format = jsonlite::unbox(format),
    predicate = concatenate_predicates(x))
    # jsonlite::toJSON()
}

#' join all queries
#' NOTE: There is a maximum of 101k entries in total. Should be possible to enforce that here
#' @noRd
#' @keywords Internal
concatenate_predicates <- function(x){

  # check for taxonomic queries
  if(!is.null(x$identify)){
    identify <- parse_predicates_identify(x$identify)
  }else{
    identify <- NULL
  }
  
  # check for spatial queries
  if(!is.null(x$geolocate)){
    location <- parse_predicates_location(x$geolocate)
  }else{
    location <- NULL
  }
  
  # parse correctly given provided information
  if(!is.null(x$filter)){
    result <- x$filter
    if(is_and_query(x)){
      result$predicates <- c(result$predicates,
                             identify,
                             location) |>
        remove_nulls_from_list()
    }else{ # filter exists, but no type (e.g. it's length-1)
      # NOTE: This code looks similar to when filter is missing
      # consolidate?
      result <- c(result, 
                  identify,
                  location) |>
        remove_nulls_from_list()
      if(length(result) > 1){
        result <- list(type = jsonlite::unbox("and"), 
                       predicates = result)
      }
    }
  # i.e. if filter is missing
  }else{
    result <- list(identify, location) |>
      remove_nulls_from_list()
    if(length(result) > 1){
      result <- list(type = jsonlite::unbox("and"), 
                     predicates = result)
    }
  }
  
  # add class and return
  class(result) <- c("galah_filter_predicate", "list")
  result
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

#' clean up a list
#' @noRd
#' @keywords Internal
remove_nulls_from_list <- function(x){
  x[!unlist(purrr::map(x, is.null))]
}

#' handle taxonomic queries
#' @noRd
#' @keywords Internal
parse_predicates_identify <- function(x){
  if(!is.null(x)){
    result <- purrr::map(x$taxon_concept_id,
               \(a){list(type = jsonlite::unbox("equals"),
                         key = jsonlite::unbox("TAXON_KEY"),
                         value = jsonlite::unbox(a))})
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
        list(type = jsonlite::unbox("geoDistance"),
             latitude = jsonlite::unbox(location$lat),
             longitude = jsonlite::unbox(location$lon),
             distance = jsonlite::unbox(paste0(location$radius, "km"))) |>
          list()
      }else{
        list(type = jsonlite::unbox("within"), 
             geometry = jsonlite::unbox(location)) |>
          list()
      }
    }else{
      list(type = jsonlite::unbox("within"), 
           geometry = jsonlite::unbox(location)) |>
        list()
    }
  }else{
    NULL
  }
}