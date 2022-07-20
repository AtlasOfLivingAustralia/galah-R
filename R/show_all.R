#' Look up record metadata
#' 
#' The living atlases store a huge amount of information, above and beyond the 
#' occurrence records that are their main output. In `galah`, users can investigate 
#' the metadata needed to build complex queries using two meta-functions or a larger 
#' number of sub-functions.
#'
#' @aliases search_all
#' @param type `string`: What type of parameters should be searched?
#' See `Details` (below) for accepted values.
#' @param field `string`: For `type = "values"` or `"profile_values"`, which
#' field or profile should information be returned for?
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble)  
#' containing all data of interest (for `show_all()`), or those that match the 
#' search query (for `search_all()`).
#' @md
#' @details There are five different categories of metadata, each with their own 
#' sub-functions. Valid `type`s are:
#' 
#' | **Category** | **Type** | **Description** | **Sub-functions** |
#' |---|---|---|---|
#' | configuration  |`"atlases"`| Show what atlases are available | [show_all_atlases()], [search_atlases()]|
#' | |`"reasons"`| Show what values are acceptable as 'download reasons' for a specified atlas | [show_all_reasons()], [search_reasons()]|
#' | taxonomy | `"taxa"` | Search for one or more taxonomic names | [search_taxa()] |
#' | |`"identifiers"`| Take a universal identifier and return taxonomic information | [search_identifiers()] |
#' | |`"ranks"`| Show valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.) | [show_all_ranks()], [search_ranks()]) |
#' | filters |`"fields"`| Show fields that are stored in an atlas | [show_all_fields()], [search_fields()] |
#'  | |`"values"`| Show the values of a particular field |[show_all_values()], [search_values()]|
#'  | |`"assertions"`| Show results of data quality checks run by each atlas | [show_all_assertions()], [search_assertions()] |
#' |group filters|`"profiles"`| Show what data profiles are available | [show_all_profiles()], [search_profiles()] |
#' | |`"profile_values"`| Show the attributes of a single profile| [show_profile_attributes()], [search_profile_attributes()] |
#' | |`"species_lists"`| Show what species lists are available||
#' |data providers|`"providers"`| Show which institutions have provided data |[show_all_providers()], [search_providers()]|
#' | |`"collections"`|Show the specific collections within those institutions| [show_all_collections()], [search_collections()]|
#' | |`"datasets"`|Shows all the data groupings within those collections|[show_all_datasets()], [search_datasets()]|   
#' 
#' Each valid `type` within `show_all` has a corresponding internal function where
#' type is used as a suffix. For example, `show_all(type = "fields")` is 
#' synonymous with `show_all_fields()`, while `search_all("basisOfRecord", type = "fields")`
#' is synonymous with `search_fields("basisOfRecord")`
#'
#' @references 
#' *  Darwin Core terms <https://dwc.tdwg.org/terms/>
#' *  ALA fields <https://api.ala.org.au/#ws72>
#' *  ALA assertions fields <https://api.ala.org.au/#ws81>
#' 
#' @seealso These functions are used to pass valid arguments to
#' [galah_select()], [galah_filter()], and related functions.
#'
#' @export

test_fun <- function(x){enquos(x)}

show_all <- function(type, field){
  # NOTE: `field` works for show_all_values() but not show_all_profile_attributes()
  
  # vector of valid types for this function
  valid_types <- c(
    "ranks",
    "fields", "values", "assertions",
    "profiles", "profile_values", "species_lists",
    "atlases", "reasons", 
    "providers", "collections", "datasets")
    # show_all_cached_files?

  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- enquos(type) |> parse_objects_or_functions()   
    type <-  gsub("\"", "", as_label(type[[1]]))
    assert_that(is.character(type))
    check_type_valid(type, valid_types)   
  }
 
  # run the appropriate function for each type
  if(type == "values" & !missing(field)){
    args <- list(field = field)
  }else{
    args <- list()
  }
  do.call(paste0("show_all_", type), args)
}


#' search atlas metadata
#' @param query `string`: A search string. Not case sensitive.
#' @rdname show_all
#' @export
search_all <- function(type, query){
  
  # vector of valid types for this function
  valid_types <- c(
    "ranks",
    "fields", "values", "assertions",
    "profiles", "profile_values", "species_lists",
    "atlases", "reasons", 
    "taxa",
    "providers", "collections", "datasets")
    # show_all_cached_files?

  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- enquos(type) |> parse_objects_or_functions()   
    type <-  gsub("\"", "", as_label(type[[1]]))
    assert_that(is.character(type))
    check_type_valid(type, valid_types)   
  }
   
  # check query
  if(missing(query)){
    abort("No query provided")
  }
  
  # run query
  do.call(paste0("search_", type), args = list(query = query))
   
}


check_type_valid <- function(type, valid, error_call = caller_env()) {
  if(!any(valid == type)){
    bullets <- c(
      glue("type `{type}` is not recognised"),
      i = "see ?show_all for a list of valid types"
    )
    abort(bullets, call = error_call)   
  }
}