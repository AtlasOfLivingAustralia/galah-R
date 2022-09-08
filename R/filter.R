#' Filter for object of class `data_request`
#' @importFrom dplyr filter
#' @rdname galah_filter
#' @export
filter.data_request <- function(data, ...){
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
  update_galah_call(data, filter = dot_parsing(dots))

}

galah_filter <- function(..., profile = NULL){
  dots <- enquos(..., .ignore_empty = "all")
  check_filter(dots)
  
  # check to see if any of the inputs are a data request
  checked_dots <- detect_data_request(dots)
  if(!inherits(checked_dots, "quosures")){
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  }else{
    is_data_request <- FALSE
  }
  
  # if a data request was supplied, return one
  if(is_data_request){
    named_filters <- dot_parsing(dots)
    # Check and apply profiles to query
    if(!is.null(profile)){
      named_filters <- apply_profiles(profile, named_filters)
    }
    update_galah_call(data_request, filter = named_filters)
  }else{
    filter.data_request(galah_call(), ...)$filter
  }
  
}

dot_parsing <- function(dots){
  
  # Clean user arguments
  if(length(dots) > 0){
    
    # First, evaluate filters that use functions (if there are any)
    dots <- parse_objects_or_functions(dots)
    named_filters <- parse_inputs(dots)
    named_filters$query <- parse_query(named_filters)
    
    # Validate that variables exist in ALA
    if (getOption("galah_config")$run_checks){     
      validate_fields(named_filters$variable)
    }
    
  }else{ 
    # If no fields are entered, return an empty data frame of arguments
    named_filters <- data.frame(variable = character(),
                     logical = character(),
                     value = character(),
                     query = character())
  }
  
  result <- tibble(named_filters)
  attr(result, "call") <- "galah_filter"
  return(result)
  
}