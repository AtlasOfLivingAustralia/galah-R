#' search atlas metadata
#' @param query `string`: A search string. Not case sensitive.
#' @rdname show_all
#' @export
search_all <- function(query, type){
  
  # vector of valid types for this function
  valid_types <- c(
    "taxa", "identifiers", "ranks", 
    "fields", "values", "assertions",
    "profiles", "profile_values", "species_lists",
    "atlases", "reasons", 
    "collections", "datasets", "providers")
   
  # check query
  if(missing(query)){
    abort("No query provided")
  }
    
  # check 'type' is ok
  if(missing(type)){type <- "fields"}
  if(length(type) > 1){type <- type[[1]]}
  assert_that(is.character(type))
  check_type_valid(type, valid_types) 
  
  # run query
  do.call(paste0("search_", type), args = list(query = query))
   
}