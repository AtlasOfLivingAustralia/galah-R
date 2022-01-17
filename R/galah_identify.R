#' Build a query that contains taxonomic identifiers
#'
#' A common task is to restrict searches to known taxonomic groups. This function
#' allows users to pass identifiers found using `search_taxa` within a pipe to 
#' provide data only for the biological group of interest.
#'
#' @param ... an object of class `ala_id`, `gbifid`, `nbnid` or `character`.
#'
#' @seealso [search_taxa()] to find identifiers from scientific names;
#' [search_identifiers()] for how to get names if taxonomic identifiers 
#' are already known.
#' 
#' @export
galah_identify <- function(...) {
  
  # check to see if any of the inputs are a data request
  dots <- enquos(..., .ignore_empty = "all")
  checked_dots <- detect_data_request(dots)
  if(!inherits(checked_dots, "quosures")){
    is_data_request <- TRUE
    data_request <- checked_dots[[1]]
    dots <- checked_dots[[2]]
  }else{
    is_data_request <- FALSE
  }
  
  # if empty, return correct class, but no values
  if(length(dots) < 1){
    result <- as_tibble(data.frame(name = character()))

  }else{
  
    # capture named inputs
    check_queries(dots) 
    
    # convert dots to query
    query <- parse_basic_quosures(dots)
    
    # check for types
    if(inherits(query, "ala_id")){
      query <- query$taxon_concept_id
    }else if(inherits(query, c("gbifid", "nbnid"))){ # from taxize
      query <- as.character(query)
    } 
    if(!inherits(query, "character")){
      bullets <- c(
        "Object passed to `galah_identify` isn't from a recognised class",
        i = "Recognised classes are `ala_id`, `gbifid`, `nbnid` or `character`",
        i = "Perhaps try using `search_taxa`?"
      )
      abort(bullets, call = caller_env())
    }
    
    result <- tibble(name = query)
  }
  
  class(result) <- append(class(result), "galah_identify") 
  
  # if a data request was supplied, return one
  if(is_data_request){
    update_galah_call(data_request, identify = result)
  }else{
    result
  }
} 


# checker function based on `galah_filter.R/check_filters`
check_queries <- function(dots, error_call = caller_env()) {
  if(any(have_name(dots))){
    bullets <- c(
      "We detected a named input.",
      i = glue("This usually means that you've used `=` somewhere"),
      i = glue("`galah_identity` doesn't require equations")
    )
    abort(bullets, call = error_call)
  }
}