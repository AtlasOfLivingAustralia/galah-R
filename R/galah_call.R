#' Start building a query
#' 
#' @description
#' To download data from the ALA (or another atlas), one must construct a data 
#' query. This query tells the atlas API what data to download and return, as 
#' well as how it should be filtered. Using `galah_call()` allows you to build 
#' a piped query to download data, in the same way that you would wrangle data 
#' with `dplyr` and the `tidyverse`.
#'
#' Since version 2.0, `galah_call()` is a wrapper to a group underlying 
#' `request_` functions. Each of these functions can begin a piped query and end 
#' with `collapse()`, `compute()` or `collect()`. 
#' 
#' The underlying `request_` 
#' functions are useful because they allow `galah` to separate different types 
#' of requests to perform better. For example, 
#' `filter.data_request` translates filters in R to `solr`, whereas 
#' `filter.metadata_request` searches using a search term.
#' 
#' @param method string: what `request` function should be called. Should be one
#' of `"data"` (default), `"metadata"` or `"files"`
#' @param type string: what form of data should be returned? Acceptable values
#' are specified by the corresponding `request` function
#' @param ... Zero or more arguments to alter a query. See 'details'.
#' 
#' @details
#' Each atlas has several types of data that can be chosen. Currently supported
#' are `"occurrences"` (the default), `"species"` and `"media"` (the latter
#' currently only for ALA). It is also possible to use  
#' `type = "occurrences-count"` and `type = "species-count"`; but in practice 
#' this is synonymous with `galah_call() |> count()`, and is 
#' therefore only practically useful for debugging (via `collapse()` and 
#' `compute()`).
#' 
#' Other named arguments are supported via `...`. In practice, functions 
#' with a `galah_` prefix and S3 methods ported from `dplyr` assign 
#' information to the correct slots internally. Overwriting these with 
#' user-defined alternatives is possible, but not advised. Accepted
#' arguments are: 
#' 
#'  - `filter` (accepts `galah_filter()` or \code{\link[=filter.data_request]{filter()}})
#'  - `select` (accepts `galah_select()` or \code{\link[=filter.data_request]{select}})
#'  - `group_by` (accepts `galah_group_by()` or \code{\link[=group_by.data_request]{group_by()}})
#'  - `identify` (accepts `galah_identify()` or \code{\link[=identify.data_request]{identify()}})
#'  - `geolocate` (accepts `galah_geolocate()`, `galah_polygon()` `galah_bbox()` or 
#'    \code{\link[=st_crop.data_request]{st_crop()}})
#'  - `limit` (accepts \code{\link[=slice_head.data_request]{slice_head()}})
#'  - `down_to` (accepts `galah_down_to()`, specific to `atlas_taxonomy()`)
#'  - `doi` (accepts a sting listing a valid DOI, specific to `collect()` when `type = "doi"`)
#'  
#' Unrecognised names are ignored by `collect()` and related functions.
#' 
#' @return Each sub-function returns a different object class: `request_data()` 
#' returns `data_request`. `request_metadata` returns `metadata_request`,
#' `request_files()` returns `files_request`.
#' @rdname galah_call
#' @examples
#' \dontrun{ 
#' # Begin your query with `galah_call()`, then pipe using `%>%` or `|>`
#' 
#' # Get number of records of *Aves* from 2001 to 2004 by year
#' galah_call() |>
#'   galah_identify("Aves") |>
#'   galah_filter(year > 2000 & year < 2005) |>
#'   galah_group_by() |>
#'   atlas_counts()
#'   
#' # Get information for all species in *Cacatuidae* family
#' galah_call() |>
#'   galah_identify("Cacatuidae") |>
#'   atlas_species()
#'   
#' # Download records of genus *Eolophus* from 2001 to 2004
#' galah_config(email = "your-email@email.com")
#' 
#' galah_call() |>
#'   galah_identify("Eolophus") |>
#'   galah_filter(year > 2000 & year < 2005) |>
#'   atlas_occurrences()
#' }
#' @export galah_call
galah_call <- function(method = c("data", "metadata", "files"),
                       type,
                       ...){
  method <- match.arg(method)
  if(missing(type)){
    type <- switch(method,
                   "data" = "occurrences", 
                   "metadata" = "fields",
                   "files" = "media")
  }
  switch(method, 
         "data" = request_data(type = type, ...),
         "metadata" = request_metadata(type = type),
         "files" = request_files(type = type))
}

#' @rdname galah_call
#' @export
request_data <- function(type = c("occurrences", 
                                  "species",
                                  "occurrences-count",
                                  "species-count",
                                  "media"),
                         ...){
  if(!missing(type)){
    type <- match.arg(type)
  }else{
    type <- "occurrences"
  }
  # create an empty list
  valid_names <- c("type", "identify", "filter", "select", "group_by",
                   "geolocate" 
                   # "order" # tentatively removed
                   )
  default_call <- vector(mode = "list", length = length(valid_names))
  names(default_call) <- valid_names
  default_call$type <- check_type(type)
  # set default for limit?
  # default_call$limit <- 100 ?
  class(default_call) <- "data_request"
  # update
  if(length(list(...)) > 0){
    update_data_request(default_call, ...)
  }else{
    default_call
  }
}

#' @rdname galah_call
#' @export
request_metadata <- function(
    type = c("fields",
             "apis",      # offline
             "assertions",
             "atlases",   # offline
             "collections",
             "datasets",
             # distributions" # new
             "layers",    # new
             "licences",
             "lists",
             "profiles",
             "providers",
             "ranks",      # offline
             "reasons",
             "taxa",
             "identifiers"
    ) 
    # note: option to add `...` here for consistency with `request_data()`
){
  x <- list(type = match.arg(type))
  class(x) <- "metadata_request"
  return(x)
}

#' @rdname galah_call
#' @export
request_files <- function(
    type = c("doi",
             "distributions",
             "media"
    ) 
    # note: option to add `...` here for consistency with `request_data()`
){
  x <- list(type = match.arg(type))
  class(x) <- "files_request"
  return(x)
}