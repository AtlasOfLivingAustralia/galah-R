#' Internal function to build APIs
#'
#' This function does a couple of things. Trivially, it looks up 
#' base URLs from an internal tibble (`node_config`), itself imported from 
#' a csv (`./data-raw/node_config.csv`). Entries in that tibble can contain
#' `glue`-like syntax for infilling new data, passed using `...`
#'
#' @importFrom glue glue_data
#' @keywords internal

atlas_url <- function(api_name, ..., quiet = FALSE, error_call = caller_env()){
  
  dots <- list(...)
  
  # run a check that a row in `node_config` matches your specification
  current_atlas <- getOption("galah_config")$atlas
  url_lookup <- node_config$api_name == api_name &
                node_config$atlas == current_atlas
  
  if(any(url_lookup)){
    string <- node_config$api_url[which(url_lookup)[1]]
    if(length(dots) > 0){
      glue_data(dots, string) |> URLencode()
    }else{
      URLencode(string)
    }
  }else{
    if(quiet){
      return(NULL)
    }else{
      bullets <- c(
        glue("The `{api_name}` API is not available for the selected atlas ({current_atlas})"),
        i = "Use `show_all_apis()` to list valid API calls"
      )
      abort(bullets, call = error_call)
    }
  }
}