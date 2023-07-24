#' new functions to start pipes for different types of data
#' 
#' Each function can begin a pipe and end with `collapse()`, `compute()` or
#' `collect()`. Having a different piping function is useful because it allows
#' later functions to have class-specific requests; e.g. `filter.data_request`
#' generates filters for `solr`, whereas `filter.metadata_request` is a search
#' function that accepts a single string.
#' 
#' More pragmatically, adding `request_metadata` enables use of `collect()` etc
#' for metadata functions, which make it easier to build and debug these APIs.
#' @return Each function returns a different object class: `request_data()` 
#' returns `data_request` (synonymous with `galah_call()`). `request_metadata`
#' returns `metadata_request`. 
#' `download_request`? `file_request`? `media_request`? Not sure on last type
#' @rdname request
#' @export
request_data <- function(type = c("occurrences", 
                                  "species",
                                  "occurrences-count",
                                  "species-count"), 
                         ...){
  type <- match.arg(type)
  # create an empty list
  valid_names <- c("type", "identify", "filter", "select", "group_by",
                   "geolocate", "limit", "doi")
  default_call <- vector(mode = "list", length = length(valid_names))
  names(default_call) <- valid_names
  default_call$type <- check_type(type)
  class(default_call) <- "data_request"
  # update
  if(length(list(...)) > 0){
    update_data_request(default_call, ...)
  }else{
    default_call
  }
}

#' @rdname request
#' @export
request_metadata <- function(
    type = c("fields",
             "apis",      # offline
             "assertions",
             "atlases",   # offline
             "collections",
             "datasets",
             "layers",    # new
             "licences",
             "lists",
             "profiles",
             "providers",
             "ranks",      # offline
             "reasons"
             ) 
    # note: option to add `...` here for consistency with `request_data()`
){
  x <- list(type = match.arg(type))
  class(x) <- "metadata_request"
  return(x)
}
