# Internal function to store objects generated from `show_all_` functions
# This increases speed by ensuring that the atlas is only queried when needed.
# When run with no arguments, it returns a list with currently stored objects.
# When a named field is given, it stores that field in options("galah_internal")
galah_internal <- function(...){
  
  # set all options
  ala_option_name <- "galah_internal"
  current_options <- getOption(ala_option_name)
  user_options <- list(...)
  default_options <- vector(mode = "list", length = 5)
  names(default_options) <- paste0("show_all_",
    c("atlases", "fields", "profiles", "ranks", "reasons"))
    
  # deal with different kinds of query
  if (length(user_options) == 0 && !is.null(current_options)) {
    return(current_options)
  }
  if (is.null(current_options)) {
    ## galah options have not been set yet, so set them to the defaults
    current_options <- default_options
    ## set the global option
    temp <- list(current_options)
    names(temp) <- ala_option_name
    options(temp)
    return(current_options)
  } else {
    # check all the options are valid, if so, set as options
    for (x in names(user_options)) {
      current_options[[x]] <- user_options[[x]]
    }
    ## set the global option
    temp <- list(current_options)
    names(temp) <- ala_option_name
  }
  options(temp)
 
}