#' Start building a query
#' 
#' @description
#' To download data from the selected atlas, one must construct a query. This 
#' query tells the atlas API what data to download and return, as well as how it 
#' should be filtered. Using `galah_call()` allows you to build a piped query to 
#' download data, in the same way that you would wrangle data with `dplyr` and 
#' the `tidyverse`.
#' @param method string: what `request` function should be called. Should be one
#' of `"data"` (default), `"metadata"` or `"files"`
#' @param type string: what form of data should be returned? Acceptable values
#' are specified by the corresponding `request` function
#' @param ... Zero or more arguments passed to  
#' \code{\link[=collapse.data_request]{collapse()}} to alter a query. Currently
#' only `mint.doi` (for occurrences) and `thumbnail` (for media downloads) are 
#' supported. Both are logical.
#' @details
#' In practice, `galah_call()` is a wrapper to a group of underlying 
#' `request_` functions, selected using the `method` argument. 
#' Each of these functions can begin a piped query and end with `collapse()`, 
#' `compute()` or `collect()`, or optionally one of the `atlas_` family of
#' functions. For more details see the object-oriented programming vignette:  
#' \code{vignette("object_oriented_programming", package = "galah")}
#' 
#' Accepted values of the `type` argument are set by the underlying `request_`
#' functions. While all accepted types can be set directly, some are affected
#' by later functions. The most common example is that adding 
#' \code{\link[=count.data_request]{count()}} to a pipe updates `type`, 
#' converting `type = "occurrences"` to `type = "occurrences-count"` (and ditto 
#' for `type = "species"`).
#' 
#' The underlying `request_` functions are useful because they allow `galah` 
#' to separate different types of requests to perform better. For example, 
#' `filter.data_request` translates filters in R to `solr`, whereas 
#' `filter.metadata_request` searches using a search term.
#' @return Each sub-function returns a different object class: `request_data()` 
#' returns `data_request`. `request_metadata` returns `metadata_request`,
#' `request_files()` returns `files_request`. These objects are list-like and
#' contain the following slots:
#' 
#'  - `filter`: edit by piping \code{\link[=filter.data_request]{filter()}} or [galah_filter()].
#'  - `select`: edit by piping \code{\link[=filter.data_request]{select}} or [galah_select()].
#'  - `group_by`: edit by piping \code{\link[=group_by.data_request]{group_by()}} or [galah_group_by()].
#'  - `identify`: edit by piping \code{\link[=identify.data_request]{identify()}} or [galah_identify()].
#'  - `geolocate`: edit by piping \code{\link[=st_crop.data_request]{st_crop()}}, 
#'    [galah_geolocate()], [galah_polygon()] or [galah_bbox()].
#'  - `limit`: edit by piping \code{\link[=slice_head.data_request]{slice_head()}}.
#'  - `doi`: edit by piping \code{\link[=filter.data_request]{filter(doi == "my-doi-here")}}.
#'  
#' @seealso [collapse.data_request()], [compute.data_request()], [collect.data_request()]
#' @rdname galah_call
#' @examples \dontrun{ 
#' # Begin your query with `galah_call()`, then pipe using `%>%` or `|>`
#' 
#' # Get number of records of *Aves* from 2001 to 2004 by year
#' galah_call() |>
#'   identify("Aves") |>
#'   filter(year > 2000 & year < 2005) |>
#'   group_by(year) |>
#'   atlas_counts()
#'   
#' # Get information for all species in *Cacatuidae* family
#' galah_call() |>
#'   identify("Cacatuidae") |>
#'   atlas_species()
#'   
#' # Download records of genus *Eolophus* from 2001 to 2004
#' galah_config(email = "your-email@email.com")
#' 
#' galah_call() |>
#'   identify("Eolophus") |>
#'   filter(year > 2000 & year < 2005) |>
#'   atlas_occurrences() # synonymous with `collect()`
#' 
#' 
#' # galah_call() is a wrapper to various `request_` functions.
#' # These can be called directly for greater specificity.
#' 
#' # Get number of records of *Aves* from 2001 to 2004 by year
#' request_data() |>
#'   identify("Aves") |>
#'   filter(year > 2000 & year < 2005) |>
#'   group_by(year) |>
#'   count() |>
#'   collect()
#' 
#' # Get information for all species in *Cacatuidae* family
#' request_data(type = "species") |>
#'   identify("Cacatuidae") |>
#'   collect()
#'   
#' # Get metadata information about supported atlases in galah
#' request_metadata(type = "atlases") |>
#'   collect()
#' 
#' }
#' @export
galah_call <- function(method = c("data", 
                                  "metadata", 
                                  "files"),
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
                                  "occurrences-count",
                                  "occurrences-doi",
                                  # "distributions",
                                  "species",
                                  "species-count"
                                  ),
                         ...){
  if(!missing(type)){
    type <- match.arg(type)
  }else{
    type <- "occurrences"
  }
  # create an empty list
  valid_names <- c("type",
                   "identify",
                   "filter",
                   "select",
                   "group_by",
                   "arrange",
                   "geolocate", 
                   "data_profile"
                   # "order" # tentatively removed
                   )
  default_call <- vector(mode = "list", length = length(valid_names))
  names(default_call) <- valid_names
  default_call$type <- type # check_type(type)
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
#' @importFrom glue glue
#' @importFrom rlang abort
#' @export
request_metadata <- function(type = c("fields",
                                      "apis",
                                      "assertions",
                                      "atlases",
                                      "collections",
                                      "datasets",
                                      # "distributions",
                                      "licences",
                                      "lists",
                                      "media",
                                      "profiles",
                                      "providers",
                                      "ranks",
                                      "reasons",
                                      "taxa",
                                      "identifiers")){
  type_checked <- try(match.arg(type),
                      silent = TRUE)
  if(inherits(type_checked, "try-error")){
    bullets <- c(
      glue("Unrecognised metadata requested."),
      i = "See `?show_all()` for a list of valid metadata types.",
      x = glue("Can't find metadata type `{type}`.")
    )
    abort(bullets)   
  }
  x <- list(type = type_checked)
  class(x) <- "metadata_request"
  return(x)
}

#' @rdname galah_call
#' @export
request_files <- function(
    type = "media"
    # note: option to add `...` here for consistency with `request_data()`
){
  x <- list(type = match.arg(type))
  class(x) <- "files_request"
  return(x)
}
