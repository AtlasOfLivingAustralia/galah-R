capture_events <- function(x,
                           error_call = rlang::caller_env()){
  browser()
}

capture_events_count <- function(x,
                                 error_call = rlang::caller_env()){
  
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
  
  list(type = "data/events-count",
       url = url_lookup("data/events-count"),
       headers =  c(build_headers(),
                    `Content-Type` = "application/json"),
       body = predicates_info) |>
    as_prequery()
}