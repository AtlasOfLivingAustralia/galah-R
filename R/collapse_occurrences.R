#' Internal function to collapse occurrences; called by `collapse_query`
#' @noRd
#' @keywords Internal
collapse_occurrences <- function(x){
  if(any(names(x) == "body")){
    result <- list(
      creator = x$request$authenticate$username,
      notificationAddresses = list(x$request$authenticate$email), 
        # note that in the above line, list() *must* be kept
        # as it wraps the address in [] and API calls fail without it.
      sendNotification = potions::pour("package", "send_email", .pkg = "galah"),
      format = x$body$format,
      predicate = build_predicates(x$body)) |>
        jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)
    x$body <- result
    x    
  }
  x
}