#' Start building a request
#' 
#' @description
#' To download data from the selected atlas, one must construct a query. This 
#' query tells the atlas API what data to download and return, as well as how it 
#' should be filtered. Using [galah_call()] allows you to build a piped query to 
#' download data, in the same way that you would wrangle data with `dplyr` and 
#' the `tidyverse`.
#' @param method string: what `request` function should be called. Should be one
#' of `"data"` (default), `"metadata"` or `"files"`
#' @param type string: what form of data should be returned? Acceptable values
#' are specified by the corresponding `request` function
#' @details
#' [galah_call()] is a wrapper to a group of underlying 
#' `request_` functions, selected using the `method` argument. 
#' Each of these functions can begin a piped query, which is then actioned using
#' \code{\link[=collect.data_request]{collect()}}, or optionally one of the 
#' \code{\link[=atlas_occurrences]{atlas_}} family of functions. For more 
#' details see the object-oriented programming vignette:  
#' \code{vignette("object_oriented_programming", package = "galah")}
#' 
#' Accepted values of the `type` argument are set by the underlying `request_`
#' functions. These functions are useful because they allow `galah` 
#' to separate different types of requests to perform better. For example, 
#' \code{\link[=filter.data_request]{filter.data_request()}} translates filters 
#' to `solr` syntax for the living atlases, or to predicates for GBIF, whereas 
#' \code{\link[=filter.metadata_request]{filter.metadata_request()}} adds a 
#' search term to your query.
#' @return Each sub-function returns a different object class: 
#' 
#' - [request_data()]  returns class `"data_request"`
#' - [request_metadata()] returns class `"metadata_request"`
#' - [request_files()] returns class `"files_request"` 
#' 
#' These objects are list-like and store later dplyr verbs in the order 
#' they are provided.
#'  
#' @seealso To amend a request object, use [apply_profile()],
#' \code{\link[=arrange.data_request]{arrange()}},
#' \code{\link[=count.data_request]{count()}},
#' \code{\link[=distinct.data_request]{distinct()}},
#' \code{\link[=filter.data_request]{filter()}},
#' \code{\link[=group_by.data_request]{group_by()}},
#' \code{\link[=identify.data_request]{identify()}},
#' \code{\link[=select.data_request]{select}},
#' \code{\link[=slice_head.data_request]{slice_head()}} or [unnest()].
#' For operations on `_request` objects, see 
#' \code{\link[=as_query.data_request]{as_query()}}, 
#' [coalesce()], 
#' \code{\link[=collapse.data_request]{collapse()}}, 
#' \code{\link[=compute.data_request]{compute()}} or 
#' \code{\link[=collect.data_request]{collect()}}.
#' @rdname galah_call
#' @examples \dontrun{ 
#' # Begin your query with `galah_call()`, then pipe using `%>%` or `|>`
#' 
#' # Get number of records of *Aves* from 2001 to 2004 by year
#' galah_call() |>
#'   identify("Aves") |>
#'   filter(year > 2000 & year < 2005) |>
#'   group_by(year) |>
#'   count() |>
#'   collect()
#'   
#' # Get information for all species in *Cacatuidae* family
#' galah_call() |>
#'   identify("Cacatuidae") |>
#'   distinct("speciesID", .keep_all = TRUE) |>
#'   collect()
#'   
#' # Download records of genus *Eolophus* from 2001 to 2004
#' galah_config(email = "your-email@email.com")
#' 
#' galah_call() |>
#'   identify("Eolophus") |>
#'   filter(year > 2000 & year < 2005) |>
#'   collect()
#' }
#' @export
galah_call <- function(method = c("data", 
                                  "metadata", 
                                  "files"),
                       type){
  method <- match.arg(method)
  if(missing(type)){
    type <- switch(method,
                   "data" = "occurrences", 
                   "metadata" = "fields",
                   "files" = "media")
  }
  switch(method, 
         "data" = request_data(type = type),
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
                                  )){
  if(!missing(type)){
    type <- match.arg(type)
  }else{
    type <- "occurrences"
  }
  list(type = type) |>
    structure(class = "data_request")
}

#' @rdname galah_call
#' @export
request_metadata <- function(type = c("fields",
                                      "apis",
                                      "assertions",
                                      "atlases",
                                      "collections",
                                      "config",
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
    c(
      "Unrecognised metadata requested.",
      i = "See `?show_all()` for a list of valid metadata types.",
      x = "Can't find metadata type `{type}`.") |>
    cli::cli_abort()   
  }
  list(type = type_checked) |>
    structure(class = "metadata_request")
}

#' @rdname galah_call
#' @export
request_files <- function(
    type = "media"
    # note: option to add `...` here for consistency with `request_data()`
){
  list(type = match.arg(type)) |>
    structure(class = "files_request")
}
