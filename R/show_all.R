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
#' `r lifecycle::badge("experimental")`
#' `show_all()` is a helper function that can display multiple types of 
#' information from `show_all_` sub-functions. 
#' See `Details` (below) for accepted values.
#' 
#' @details There are five categories of information, each with their own 
#' specific sub-functions to look-up each type of information. 
#' The available types of information for `show_all_` are:
#' 
#' | **Category** | **Type** | **Description** | **Sub-functions** |
#' |---|---|---|---|
#' | configuration  |`atlases`| Show what atlases are available | `show_all_atlases()` |
#' | |`apis`| Show what APIs & functions are available for each atlas | `show_all_apis()` |
#' | |`reasons`| Show what values are acceptable as 'download reasons' for a specified atlas | `show_all_reasons()` |
#' | taxonomy | `ranks`| Show valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.) | `show_all_ranks()` |
#' | filters |`fields`| Show fields that are stored in an atlas | `show_all_fields()` |
#' | |`assertions`| Show results of data quality checks run by each atlas | `show_all_assertions()` |
#' | |`licenses`| Show what copyright licenses are applied to media | `show_all_licenses()` |
#' |group filters|`profiles`| Show what data profiles are available | `show_all_profiles()` |
#' | |`lists`| Show what species lists are available| `show_all_lists()` |
#' |data providers|`providers`| Show which institutions have provided data | `show_all_providers()` |
#' | |`collections`|Show the specific collections within those institutions| `show_all_collections()` |
#' | |`datasets`|Shows all the data groupings within those collections| `show_all_datasets()` |   
#' 
#' 
#' @aliases show_all
#' @aliases show_all_assertions show_all_atlases 
#' @aliases show_all_collections show_all_datasets show_all_providers 
#' @aliases show_all_fields show_all_reasons show_all_ranks show_all_profiles 
#' @aliases show_all_licences show_all_apis
#' @param type A string to specify what type of parameters should be shown.
#' @param limit Optional number of values to return. Defaults to NULL, i.e. all records
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) 
#' containing all data of interest.
#' @references 
#' *  Darwin Core terms <https://dwc.tdwg.org/terms/>
#' 
#' @seealso Use the [search_all()] function and `search_()` sub-functions to 
#' search for information. These functions are used to pass valid arguments to
#' [galah_select()], [galah_filter()], and related functions.
#' @examples
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
#' @export
show_all <- function(type, limit = NULL){
  
  if(!is.null(limit)){
    if(!(is.integer(limit) | is.numeric(limit))){
      abort("limit must be either numeric or an integer")
    }
  }
  
  type_parsed <- parse_quosures_basic(enquos(type))$data
  request_metadata(type = type_parsed) |> 
    collect()
}

#' @rdname show_all
#' @importFrom tibble tibble
#' @export
show_all_assertions <- function(limit = NULL){
  request_metadata(type = "assertions") |> 
  collect()
}

#' @export show_all_atlases
#' @rdname show_all
show_all_atlases <- function(limit = NULL) {
  request_metadata(type = "atlases") |> 
  collect()
}

#' @rdname show_all
#' @export
show_all_apis <- function(limit = NULL){
  request_metadata(type = "apis") |> 
  collect()
}

#' @rdname show_all
#' @export
show_all_collections <- function(limit = NULL){
  request_metadata(type = "collections") |> 
  collect()
}

#' @rdname show_all
#' @export
show_all_datasets <- function(limit = NULL){
  request_metadata(type = "datasets") |> 
  collect()
}

#' @rdname show_all
#' @export
show_all_providers <- function(limit = NULL){
  request_metadata(type = "providers") |> 
  collect()
}

#' @rdname show_all
#' @importFrom potions pour
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @export
show_all_fields <- function(limit = NULL){
  update_needed <- internal_cache_update_needed("show_all_fields")
  if(update_needed){
    list(
      fields = collect(request_metadata(type = "fields")),
      layers = collect(request_metadata(type = "layers")),
      media = galah_internal_archived$media_fields,
      other = galah_internal_archived$other_fields) |>
    bind_rows() |>
    select(-source_link) |>
    filter(!duplicated(id))
  }else{
    check_internal_cache()$show_all_fields
  }
}

#' @rdname show_all
#' @importFrom dplyr bind_rows
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#' @export
show_all_licences <- function(limit = NULL){
  request_metadata(type = "licences") |> 
  collect()
}

#' @rdname show_all
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom potions pour
#' @export
show_all_reasons <- function(limit = NULL){
  request_metadata(type = "reasons") |> 
  collect()
}

#' @rdname show_all
#' @importFrom tibble tibble
#' @export
show_all_ranks <- function(limit = NULL) {
  request_metadata(type = "ranks") |> 
  collect() 
}

#' @rdname show_all
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @export
show_all_profiles <- function(limit = NULL) {
  request_metadata(type = "profiles") |> 
  collect()
}

#' @rdname show_all
#' @importFrom tibble tibble
#' @export
show_all_lists <- function(limit = NULL){
  request_metadata(type = "lists") |> 
  collect()
}
