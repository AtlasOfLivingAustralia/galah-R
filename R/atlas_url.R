atlas_url <- function(api_name, ..., error_call = caller_env()){
  
  # run a check that a row in `node_config` matches your specification
  current_atlas <- getOption("galah_config")$atlas
  url_lookup <- node_config$api_name == api_name &
                node_config$atlas == current_atlas
  
  if(any(url_lookup)){
    glue(node_config$api_url[which(url_lookup)[1]])
  }else{
    bullets <- c(
      glue("The `{api_name}` API is not available for the selected atlas ({current_atlas})"),
      i = "Use `show_all_apis()` to list valid API calls"
    )
    abort(bullets, call = error_call)
  }
}