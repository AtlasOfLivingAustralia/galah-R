#' Internal function to collapse occurrences; called by `collapse_query`
#' @noRd
#' @keywords Internal
collapse_occurrences <- function(x){
  if(any(names(x) == "body")){
    result <- list(
      creator = potions::pour("user", "username", .pkg = "galah"),
      notificationAddresses = potions::pour("user", "email", .pkg = "galah"),
      sendNotification = potions::pour("package", "send_email", .pkg = "galah"),
      format = x$format,
      predicate = build_predicates(x$body)) |>
      jsonlite::toJSON(auto_unbox = TRUE,
                       pretty = TRUE)
    x$body <- result
    x    
  }
  x
}