#' @rdname show_values
#' @param list `string`: list to return information from
#' @export show_list_values

show_list_values <- function(list){
  
  if (missing(list)) {
    bullets <- c(
      "No field detected.",
      i = "Did you forget to add a list to show values for?"
    )
    abort(bullets, call = caller_env())
  }
  
  atlas_url("lists_lookup", list_id = list) |> 
    atlas_GET() |>
    tibble()
}