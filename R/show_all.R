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
  
  # vector of valid types for this function
  valid_types <- c(
    "ranks",
    "fields", "assertions",
    "licences",
    "profiles", "lists",
    "atlases", "apis", "reasons", 
    "providers", "collections", "datasets")
  
  # check 'type' is ok
  if(missing(type)){
    type <- "fields"
  }else{
    type <- parse_quosures_basic(enquos(type))$data
    assert_that(is.character(type))
    check_type_valid(type, valid_types)   
  }
  
  do.call(paste0("show_all_", type), args = list(limit = limit))
}


#' @rdname show_all
#' @importFrom tibble tibble
#' @export
show_all_assertions <- function(limit = NULL){
  if(is_gbif()){
    result <- gbif_internal_archived$assertions
  }else{
    url <- url_lookup("records_assertions") 
    assertions <- query_API(url) |> bind_rows()
    if(is.null(assertions)){
      result <- tibble()
    }else{
      if(!is.null(limit)){
        limit_rows <- seq_len(min(nrow(assertions), limit))
        assertions <- assertions[limit_rows, ]
      }
      assertions$data_type <- "logical"
      names(assertions) <- rename_columns(names(assertions), type = "assertions")
      assertions <- assertions[wanted_columns("assertions")]
      assertions$type <- "assertions"
      result <- assertions
    }
  }
  attr(result, "call") <- "show_all_assertions"
  return(result)
}


#' @export show_all_atlases
#' @rdname show_all
show_all_atlases <- function(limit = NULL) {
  df <- node_metadata
  if(!is.null(limit)){
    limit_rows <- seq_len(min(nrow(df), limit))
    df <- df[limit_rows, ]
  }
  attr(df, "call") <- "show_all_atlases"
  return(df)
  }

#' @rdname show_all
#' @export
show_all_apis <- function(limit = NULL){
  df <- node_config
  if(!is.null(limit)){
    limit_rows <- seq_len(min(nrow(df), limit))
    df <- df[limit_rows, ]
  }
  attr(df, "call") <- "show_all_apis"
  return(df)
}

#' @rdname show_all
#' @export
show_all_collections <- function(limit = NULL){
  collectory_funs(limit, 
                  url_tag = "collections_collections", 
                  fun_name = "collections")
}

#' @rdname show_all
#' @export
show_all_datasets <- function(limit = NULL){
  collectory_funs(limit, 
                  url_tag = "collections_datasets", 
                  fun_name = "datasets")
}

#' @rdname show_all
#' @export
show_all_providers <- function(limit = NULL){
  collectory_funs(limit, 
                  url_tag = "collections_providers", 
                  fun_name = "providers")
}


#' Internal function to handle collectories
#' @noRd
#' @keywords Internal
#' @importFrom tibble tibble
collectory_funs <- function(limit = NULL, url_tag, fun_name){
  # set behaviour for gbif versus elsewhere
  if(is.null(limit)){
    limit <- 100
  }
  if(is_gbif()){
    slot_name <- "results"
    limit_name <- "limit"
  }else{
    slot_name <- NULL
    limit_name <- "max"
  }
  
  # get url, run
  url <- url_lookup(url_tag)
  df <- url_paginate(url,
                     group_size = 500,
                     limit_name = limit_name,
                     limit = limit,
                     slot_name = slot_name)
  
  # set attributes
  if(is.null(df)){
    paste0("show_all(", fun_name, ")") |>
      system_down_message()
  }else{
    if(!is.null(limit)){
      limit_rows <- seq_len(min(nrow(df), limit))
      df <- df[limit_rows, ]
    }
    df <- tibble(df)
    attr(df, "call") <- paste0("show_all_", fun_name)
    return(df)    
  }
}


#' @rdname show_all
#' @importFrom potions pour
#' @export
show_all_fields <- function(limit = NULL){
  
  # check whether the cache has been updated this session
  atlas <- pour("atlas", "region")
  update_needed <- internal_cache_update_needed("show_all_fields")
  
  if(update_needed){ # i.e. we'd like to run a query
    if(is_gbif()){
      df <- gbif_internal_archived$fields # slightly untidy solution to GBIF hard-coded fields
      attr(df, "call") <- "show_all_fields"
      return(df)
    }else{
      fields <- get_fields()
      layers <- get_layers()
      media <- get_media()
      other <- get_other_fields()
      if(all(
        is.null(fields),
        is.null(layers)
      )){
        df <- NULL
      }else{
        df <- list(fields[!(fields$id %in% layers$id), ], layers, media, other) |>
          bind_rows()
      }
    
      # if calling the API fails
      if(is.null(df)){ 
        df <- check_internal_cache()$show_all_fields
        # if cached values reflect the correct atlas, return requested info
        if(attr(df, "atlas_name") == atlas){ 
          attr(df, "ARCHIVED") <- NULL # remove identifying attributes
          # otherwise return a message
        }else{ 
          bullets <- c(
            "Calling the API failed for `show_all_fields`.",
            i = "This might mean that the system is down."
          )
          inform(bullets)
          df <- tibble()
        }
      
       # if the API call worked
      }else{ 
        attr(df, "atlas_name") <- atlas
        check_internal_cache(show_all_fields = df)
        attr(df, "call") <- "show_all_fields"
      }
    } # end if not gbif
    
    # if no update needed
  }else{    
    df <- check_internal_cache()$show_all_fields
    attr(df, "call") <- "show_all_fields"
  }   
  
  if(!is.null(limit)){
    limit_rows <- seq_len(min(nrow(df), limit))
    df <- df[limit_rows, ]
  }
  return(df)
}

#' @rdname show_all
#' @importFrom dplyr bind_rows
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#' @export
show_all_licences <- function(limit = NULL){
  url <- url_lookup("image_licences")
  df <- query_API(url) |> 
        bind_rows() |>
        select(all_of(c("id", "name", "acronym", "url")))
  if(!is.null(limit)){
    limit_rows <- seq_len(min(nrow(df), limit))
    df <- df[limit_rows, ]
  }
  attr(df, "call") <- "show_all_licences"
  return(df)
}

#' @rdname show_all
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom potions pour
#' @export
show_all_reasons <- function(limit = NULL){
  
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_reasons")
  atlas <- pour("atlas", "region")
  
  if(update_needed){ # i.e. we'd like to run a query
    ## return list of valid "reasons for use" codes
    url <- url_lookup("logger_reasons")
    out <- query_API(url) |>
           bind_rows()
    if(is.null(out)){
      df <- check_internal_cache()$show_all_reasons
      # if cached values reflect the correct atlas, return requested info
      if(attr(df, "atlas_name") == atlas){ 
        df <- check_internal_cache()$show_all_reasons
        attr(df, "ARCHIVED") <- NULL # remove identifying attributes
        # otherwise return a message
      }else{ 
        bullets <- c(
          "Calling the API failed for `show_all_reasons`.",
          i = "This might mean that the system is down."
        )
        inform(bullets)
        df <- tibble()
      }
      # if the API call works
    }else{
      df <- out |> 
        filter(!deprecated) |>
        select(all_of(wanted_columns("reasons"))) |>
        arrange(id)
      attr(df, "atlas_name") <- atlas
      attr(df, "call") <- "show_all_reasons"
    }
    # if no update is needed
  }else{
    attr(df, "call") <- "show_all_reasons"
    df <- check_internal_cache()$show_all_reasons
  }
  
  if(!is.null(limit)){
    limit_rows <- seq_len(min(nrow(df), limit))
    df <- df[limit_rows, ]
  }
  return(df)
}


#' @rdname show_all
#' @importFrom tibble tibble
#' @export
show_all_ranks <- function(limit = NULL) {
  if(is_gbif()){
    df <- tibble(
      id = seq_len(9),
      name = c("kingdom", "phylum", "class", 
               "order", "family", "genus", 
               "species", "subspecies", "infraspecific")
    )
  }else{
    df <- tibble(
      id = seq_len(69),
      name = c("root", "superkingdom", "kingdom", "subkingdom",
               "superphylum", "phylum", "subphylum", "superclass", "class", 
               "subclass", "infraclass", "subinfraclass", 
               "superdivison zoology", "division zoology", 
               "subdivision zoology", "supercohort", "cohort", "subcohort", 
               "superorder", "order", "suborder", "infraorder", "parvorder", 
               "superseries zoology", "series zoology", "subseries zoology", 
               "supersection zoology", "section zoology", 
               "subsection zoology", "superfamily", "family", "subfamily", 
               "infrafamily", "supertribe", "tribe", "subtribe", 
               "supergenus", "genus group", "genus", "nothogenus", 
               "subgenus", "supersection botany", "section botany", 
               "subsection botany", "superseries botany", "series botany", 
               "subseries botany", "species group", "superspecies", 
               "species subgroup", "species", "nothospecies", "holomorph", 
               "anamorph", "teleomorph", "subspecies", "nothosubspecies", 
               "infraspecies", "variety", "nothovariety", "subvariety", 
               "form", "nothoform", "subform", "biovar", "serovar", 
               "cultivar", "pathovar", "infraspecific")
    )
  }
  attr(df, "call") <- "show_all_ranks"
  if(!is.null(limit)){
    limit_rows <- seq_len(min(nrow(df), limit))
    df <- df[limit_rows, ]
  }
  return(df)
}


#' @rdname show_all
#' @importFrom dplyr all_of
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @export
show_all_profiles <- function(limit = NULL) {
  
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_profiles")
  
  if(update_needed){ # i.e. we'd like to run a query
    # return only enabled profiles?
    url <- url_lookup("profiles_all")
    resp <- query_API(url)
    if(is.null(resp)){ # if calling the API fails, return cached data
      df <- check_internal_cache()$show_all_profiles
      attr(df, "ARCHIVED") <- NULL # remove identifying attributes
    }else{
      df <- lapply(resp, function(a){a[names(a) != "categories"]}) |>
        bind_rows() |>
        select(all_of(wanted_columns(type = "profile")))
      check_internal_cache(show_all_profiles = df)
    }    
  }else{
    df <- check_internal_cache()$show_all_profiles
  }
  attr(df, "call") <- "show_all_profiles"
  if(!is.null(limit)){
    limit_rows <- seq_len(min(nrow(df), limit))
    df <- df[limit_rows, ]
  }
  df
}


#' @rdname show_all
#' @importFrom tibble tibble
#' @export
show_all_lists <- function(limit = NULL){
  if(is.null(limit)){limit <- 5000}
  
  url <- url_lookup("lists_all")
  resp <- query_API(url)
  
  # note: resp$listCount == 1107, but only 25 returned
  # pinging https://api.ala.org.au/specieslist/ws/speciesList&max=5000
  # returns $message
  # [1] "Missing Authentication Token"
  
  # df <- url_paginate(
  #   url = url_lookup("lists_all"),
  #   group_size = 1000,
  #   limit = limit,
  #   slot_name = "lists")
  if(is.null(df)){
    return(tibble())
  }else{
    df <- resp[["lists"]] |> bind_rows()
    attr(df, "call") <- "show_all_lists"
    return(df)
  }
}
