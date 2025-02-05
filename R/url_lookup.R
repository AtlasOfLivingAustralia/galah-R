#' Internal function to build APIs
#'
#' This function does a couple of things. Trivially, it looks up 
#' base URLs from an internal tibble (`node_config`), itself imported from 
#' a csv (`./data-raw/node_config.csv`). Entries in that tibble can contain
#' `glue`-like syntax for infilling new data, passed using `...`.
#'
#' Note that this function is unusual in having a `quiet` arg instead of 
#' calling `verbose` via `pour`. This is so the developer can suppress messages
#' independently of user preferences, since this function is often called 
#' internally.
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom glue glue_data
#' @importFrom glue glue
#' @importFrom potions pour
#' @importFrom rlang abort
#' @noRd
#' @keywords internal
url_lookup <- function(type,
                       ..., 
                       quiet = FALSE, 
                       error_call = caller_env()){
  dots <- list(...)
  current_atlas <- pour("atlas", "region")
  # get requested url
  if(missing(type)){
    type <- dots$type
  }
  url_string <- node_config |>
    filter(node_config$type == {{type}},
           node_config$atlas == {{current_atlas}}) |>
    pull(url)
  # parse as needed
  if(length(url_string) > 0){
    if(length(dots) > 0){
      glue_data(dots, url_string) |> 
        as.character() |>
        utils::URLencode()
    }else{
      url_string |> utils::URLencode()
    }
  }else{
    if(quiet){
      return(NULL)
    }else{
      bullets <- c(
        glue("No API is available for type `{type}`"),
        i = glue("Selected atlas: {current_atlas}"),
        i = "Use `show_all_apis()` to list valid API calls")
      abort(bullets, call = error_call)
    }
  }
}