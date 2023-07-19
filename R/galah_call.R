#' Start building a data query
#' 
#' To download data from the ALA (or another atlas), one must construct a data 
#' query. This query tells the atlas API what data to download and return, as 
#' well as how it should be filtered. Using `galah_call()` allows you to build 
#' a piped query to download data, in the same way that you would wrangle data 
#' with `dplyr` and the `tidyverse`.
#' 
#' @param type String: what form of data should be returned? Should be one of 
#' `"occurrences"`, `"species"` or `"media"`.
#' @param ... Zero or more arguments to alter a query. See 'details'.
#' @details
#' Each atlas has several types of data that can be chosen. Currently supported
#' are `"occurrences"` (the default), `"species"` and `"media"` (the latter
#' currently only for ALA). It is also possible to use  
#' `type = "occurrences-count"`; but in practice this is 
#' synonymous with `galah_call(type = "occurrences") |> count()`, and is 
#' therefore only practically useful for debugging (via `collapse()` and 
#' `compute()`). Ditto for `type = "species-count"`.
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
#'  - `media` (accepts `galah_media()`)
#'  - `down_to` (accepts `galah_down_to()`, specific to `atlas_taxonomy()`)
#'  - `doi` (accepts a sting listing a valid DOI, specific to `collect()` when `type = "doi"`)
#'  
#'  Unrecognised names are ignored by `collect()` and related functions.
#' 
#' @return An object of class `data_request`.
#' @examples
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
#' \dontrun{ 
#' # Download records of genus *Eolophus* from 2001 to 2004
#' galah_config(email = "your-email@email.com")
#' 
#' galah_call() |>
#'   galah_identify("Eolophus") |>
#'   galah_filter(year > 2000 & year < 2005) |>
#'   atlas_occurrences()
#' }
#' @export galah_call
galah_call <- function(type = "occurrences",
                       ...){
  request_data(type = type, ...)
}