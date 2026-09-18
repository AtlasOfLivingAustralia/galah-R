#' Internal function to run `capture()` for type = "events"
#' @noRd
#' @keywords Internal
capture_events <- function(x,
                           error_call = rlang::caller_env()){
  x # placeholder, obvs
}

#' Internal function to run `capture()` for type = "events-count"
#' @noRd
#' @keywords Internal
capture_events_count <- function(x,
                                 error_call = rlang::caller_env()){
  
  x$type <- "data/events-count"
   
  # compile supplied arguments into a list
  # honestly this is a little messy, but the alternative is to call 
  # [build_predicates()], which is messier as taxonomic info hasn't yet been 
  # parsed. Instead we call [build_predicates()] during [collapse_query()].
  predicates_info <- list(identify = x$identify, 
                          filter = x$filter, 
                          geolocate = x$geolocate,
                          group_by = x$group_by,
                          slice = ifelse(is.null(x$slice),
                                         tibble::tibble(slice_n = 30, slice_called = FALSE),
                                         x$slice), 
                          limit = 0)
  
  list(type = x$type,
       atlas = x$atlas,
       url = url_lookup(x),
       headers =  c(build_headers(),
                    `Content-Type` = "application/json"),
       body = predicates_info) |>
    as_prequery()
}

#' Internal function to run `capture()` for type = "events-describe"
#' @noRd
#' @keywords Internal
capture_events_describe <- function(x,
                                    error_call = rlang::caller_env()){
   x$type <- "data/events-describe"
   list(type = x$type,
        atlas = x$atlas,
        url = url_lookup(x),
        headers =  c(build_headers(),
                     `Content-Type` = "application/json"),
        body =  "query { __schema { types { name kind description } } }") |>
     as_query()
}