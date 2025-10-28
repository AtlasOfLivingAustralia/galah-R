#' @name show_all
#' @rdname show_all
#' @title Show valid record information
#' @description 
#' The living atlases store a huge amount of information, above and beyond the 
#' occurrence records that are their main output. In `galah`, one way that 
#' users can investigate this information is by showing all the available 
#' options or categories for the type of information they are interested in. 
#' Functions prefixed with `show_all_` do this, displaying all valid options 
#' for the information specified by the suffix. 
#' 
#' `r lifecycle::badge("stable")`
#' `show_all()` is a helper function that can display multiple types of 
#' information from `show_all_` sub-functions. 
#' @param ... String showing what type of information is to be requested. See 
#' `Details` (below) for accepted values.
#' @param limit Optional number of values to return. Defaults to NULL, i.e. all records
#' @param all_fields `r lifecycle::badge("experimental")` If `TRUE`, 
#'   `show_values()` also returns all columns available from the API, rather
#'   than the 'default' columns traditionally provided via galah.
#' @details There are five categories of information, each with their own 
#' specific sub-functions to look-up each type of information. 
#' The available types of information for `show_all_` are:
#' 
#' | **Category** | **Type** | **Description** | **Sub-functions** |
#' |---|---|---|---|
#' | Configuration  |`atlases`| Show what atlases are available | `show_all_atlases()` |
#' | |`apis`| Show what APIs & functions are available for each atlas | `show_all_apis()` |
#' | |`config`| Show information necessary for authentication | `show_all_config()`|
#' | |`reasons`| Show what values are acceptable as 'download reasons' for a specified atlas | `show_all_reasons()` |
#' | Data providers|`providers`| Show which institutions have provided data | `show_all_providers()` |
#' | |`collections`|Show the specific collections within those institutions| `show_all_collections()` |
#' | |`datasets`|Shows all the data groupings within those collections| `show_all_datasets()` |  
#' | Filters |`assertions`| Show results of data quality checks run by each atlas | `show_all_assertions()` |
#' | |`fields`| Show fields that are stored in an atlas | `show_all_fields()` |
#' | |`licenses`| Show what copyright licenses are applied to media | `show_all_licenses()` |
#' | |`profiles`| Show what data profiles are available | `show_all_profiles()` |
#' | Taxonomy |`lists`| Show what species lists are available| `show_all_lists()` |
#' | |`ranks`| Show valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.) | `show_all_ranks()` |
#' 
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) 
#' containing all data of interest.
#' @references 
#' *  Darwin Core terms <https://dwc.tdwg.org/terms/>
#' @seealso Use the [search_all()] function and `search_()` sub-functions to 
#' search for information. These functions are used to pass valid arguments to
#' \code{\link[=filter.data_request]{filter()}}, 
#' \code{\link[=select.data_request]{select()}}, and related functions.
#' @examples \dontrun{
#' # See all supported atlases
#' show_all(atlases)
#'
#' # Show a list of all available data quality profiles
#' show_all(profiles)
#' 
#' # Show a listing of all accepted reasons for downloading occurrence data
#' show_all(reasons)
#' 
#' # Show a listing of all taxonomic ranks
#' show_all(ranks)
#' 
#' # `show_all()` is synonymous with `request_metadata() |> collect()`
#' request_metadata(type = "fields") |>
#'   collect()
#'   
#' # using `all_fields = TRUE` is synonymous with `select(everything())`
#' request_metadata(type = "fields") |>
#'   select(everything()) |>
#'   collect()
#' }
#' @export
show_all <- function(...,
                     limit = NULL,
                     all_fields = FALSE){
  dots <- rlang::enquos(..., .ignore_empty = "all")
  if(length(dots) < 1){
    type_text <- "fields"
  }else{
    type_text <- gsub("\"", 
                      "", 
                      rlang::as_label(dots[[1]])) # handle case where type is quoted
  }
  show_all_generic(type = type_text, 
                   limit = limit,
                   all_fields = all_fields)
}

#' Internal function to handle `show_all` calls
#' This is needed to handle slight differences between syntax of 
#' `show_all()` and `collect()`
#' @noRd
#' @keywords Internal
show_all_generic <- function(type,
                             limit,
                             all_fields){
  x <- request_metadata(type = type)
  if(!is.null(limit)){
    x <- x |> dplyr::slice_head(n = limit)
  }
  if(isTRUE(all_fields)){
    x <- x |> dplyr::select(tidyselect::everything())
  }
  result <- collect(x)
  # `show_all()` always returns requested number of records
  # this differs from `collect()` which always returns what the API gives you
  if(!is.null(limit)){
    if(nrow(result) > limit){
      result <- dplyr::slice_head(result, n = limit)
    }
  }
  result
}

#' @rdname show_all
#' @export
show_all_apis <- function(limit = NULL,
                          all_fields = FALSE){
  show_all_generic(type = "apis",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_assertions <- function(limit = NULL,
                                all_fields = FALSE){
  show_all_generic(type = "assertions",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_atlases <- function(limit = NULL,
                             all_fields = FALSE){
  show_all_generic(type = "atlases",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_collections <- function(limit = NULL,
                                 all_fields = FALSE){
  show_all_generic(type = "collections",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_config <- function(){
  show_all_generic(type = "config",
                   limit = NULL,
                   all_fields = TRUE)
}

#' @rdname show_all
#' @export
show_all_datasets <- function(limit = NULL,
                              all_fields = FALSE){
  show_all_generic(type = "datasets",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @noRd
#' @keywords Internal
show_all_distributions <- function(limit = NULL,
                                   all_fields = FALSE){
  show_all_generic(type = "distributions",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_fields <- function(limit = NULL,
                            all_fields = FALSE){
  show_all_generic(type = "fields",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_licences <- function(limit = NULL,
                              all_fields = FALSE){
  show_all_generic(type = "licences",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_lists <- function(limit = NULL,
                           all_fields = FALSE){
  show_all_generic(type = "lists",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_profiles <- function(limit = NULL,
                              all_fields = FALSE){
  show_all_generic(type = "profiles",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_providers <- function(limit = NULL,
                               all_fields = FALSE){
  show_all_generic(type = "providers",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_ranks <- function(limit = NULL,
                           all_fields = FALSE){
  show_all_generic(type = "ranks",
                   limit = limit,
                   all_fields = all_fields)
}

#' @rdname show_all
#' @export
show_all_reasons <- function(limit = NULL,
                             all_fields = FALSE){
  show_all_generic(type = "reasons",
                   limit = limit,
                   all_fields = all_fields)
}