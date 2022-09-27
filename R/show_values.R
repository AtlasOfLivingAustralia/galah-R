#' Show values
#' 
#' In early development
#' @export
show_values <- function(df, type, entry){
  
  if(!grepl("^search_", attr(df, "call")){
    abort("`show_values` requires an input from `search_`")
  }

  if(!(attr(df, "call") %in% c("search_fields", "search_profiles", "search_lists"))){
    abort("can only search from `fields`, `profiles` or `lists`")
  }


  # vector of valid types for this function
  valid_types <- c("field", "profile", "list")

  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- enquos(type) |> parse_objects_or_functions()   
    type <-  gsub("\"", "", as_label(type[[1]]))
    assert_that(is.character(type))
    check_type_valid(type, valid_types)   
  }
  
  # use do.call to implement sub-function
  args <- list(entry)
  names(args)[[1]] <- type
  do.call(paste0("show_", type, "_values"), args)

}