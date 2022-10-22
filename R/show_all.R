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
#' @aliases show_all_assertions show_all_atlases show_all_cached_files 
#' @aliases show_all_collections show_all_datasets show_all_providers 
#' @aliases show_all_fields show_all_reasons show_all_ranks show_all_profiles 
#' @aliases show_all_licences show_all_apis
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) 
#' containing all data of interest.
#' @references 
#' *  Darwin Core terms <https://dwc.tdwg.org/terms/>
#' *  ALA fields <https://api.ala.org.au/#ws72>
#' *  ALA assertions fields <https://api.ala.org.au/#ws81>
#' 
#' @seealso Use the [search_all()] function and `search_()` sub-functions to 
#' search for information. These functions are used to pass valid arguments to
#' [galah_select()], [galah_filter()], and related functions.
#' @examples
#' 
#' # See all supported atlases
#' show_all(atlases)
#'
#' # Show a listing of all accepted reasons for downloading occurrence data
#' show_all(reasons)
#'
#' # Show a list of all available data quality profiles
#' show_all(profiles)
#' 
#' # Show a listing of all taxonomic ranks
#' show_all(ranks)
#' 
#' 
#' @md
NULL

#' @rdname show_all
#' @param type A string to specify what type of parameters should be shown.
#' @export show_all
show_all <- function(type){
  
  # vector of valid types for this function
  valid_types <- c(
    "ranks",
    "fields", "assertions",
    "licences",
    "profiles", "lists",
    "atlases", "apis", "reasons", 
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
  # if(type == "values" & !missing(field)){
    # args <- list(field = field)
  # }else{
    args <- list()
  # }
  
  df <- do.call(paste0("show_all_", type), args)
  
  return(df)
}


#' @rdname show_all
#' @export show_all_assertions
show_all_assertions <- function(){
  url <- atlas_url("records_assertions") 
  assertions <- atlas_GET(url)
  if(is.null(assertions)){
    result <- tibble()
  }else{
    assertions$data_type <- "logical"
    names(assertions) <- rename_columns(names(assertions), type = "assertions")
    assertions <- assertions[wanted_columns("assertions")]
    assertions$type <- "assertions"
    result <- as_tibble(assertions)
  }
  attr(result, "call") <- "show_all_assertions"
  return(result)
}


#' @export show_all_atlases
#' @rdname show_all
show_all_atlases <- function() {
  df <- node_metadata
  attr(df, "call") <- "show_all_atlases"
  return(df)
  }


#' @rdname show_all
#' @export show_all_cached_files
show_all_cached_files <- function() {
  # return a data.frame of all cached files
  metadata_path <- file.path(getOption("galah_config")$cache_directory,
                             "metadata.rds")
  if (!file.exists(metadata_path)) {
    directory <- getOption("galah_config")$cache_directory
    inform(glue("No cached file information was found in {directory}."))
    return()
  }
  readRDS(metadata_path)
}


#' @rdname show_all
#' @export show_all_apis
show_all_apis <- function(){
  df <- node_config
  attr(df, "call") <- "show_all_apis"
  return(df)
}


#' @rdname show_all
#' @export show_all_collections
show_all_collections <- function(){
  url <- atlas_url("collections_collections")
  df <- atlas_GET(url) |> tibble()
  attr(df, "call") <- "show_all_collections"
  return(df)
}


#' @rdname show_all
#' @export show_all_datasets
show_all_datasets <- function(){
  url <- atlas_url("collections_datasets")
  df <- atlas_GET(url) |> tibble()
  attr(df, "call") <- "show_all_datasets"
  return(df)
}


#' @rdname show_all
#' @export show_all_providers
show_all_providers <- function(){
  url <- atlas_url("collections_providers")
  df <- atlas_GET(url) |> tibble()
  attr(df, "call") <- "show_all_providers"
  return(df)
}


#' @rdname show_all
#' @export show_all_fields
show_all_fields <- function(){
  
  # check whether the cache has been updated this session
  atlas <- getOption("galah_config")$atlas
  update_needed <- internal_cache_update_needed("show_all_fields")
  
  if(update_needed){ # i.e. we'd like to run a query
    # if(atlas == "Global"){
    #   df <- gbif_fields() # slightly untidy solution to GBIF hard-coded fields
    # }else{
    fields <- get_fields()
    layers <- get_layers()
    media <- get_media()
    other <- get_other_fields()
    if(all(
      is.null(fields),
      is.null(layers),
      # is.null(media) # internally generated
      is.null(other)
    )){
      df <- NULL
    }else{
      df <- as.data.frame(
        rbindlist(
          list(
            fields[!(fields$id %in% layers$id), ],
            layers, media, other), 
          fill = TRUE)
      )
      # }
    }
    
    # if calling the API fails
    if(is.null(df)){ 
      df <- galah_internal_cache()$show_all_fields
      # if cached values reflect the correct atlas, return requested info
      if(attr(df, "atlas_name") == atlas){ 
        attr(df, "ARCHIVED") <- NULL # remove identifying attributes
        return(df)
        # otherwise return a message
      }else{ 
        bullets <- c(
          "Calling the API failed for `show_all_fields`.",
          i = "This might mean that the system is down."
        )
        inform(bullets)
        return(tibble())
      }
      
      # if the API call worked
    }else{ 
      df <- as_tibble(df)
      attr(df, "atlas_name") <- atlas
      galah_internal_cache(show_all_fields = df)
      attr(df, "call") <- "show_all_fields"
      return(df)
    }
    
    # if no update needed
  }else{    
    df <- galah_internal_cache()$show_all_fields
    attr(df, "call") <- "show_all_fields"
    return(df)
  }   
}

#' @rdname show_all
#' @export show_all_licences
show_all_licences <- function(){
  url <- atlas_url("image_licences")
  result <- atlas_GET(url) |> tibble()
  df <- result[, c("id", "name", "acronym", "url")] 
  attr(df, "call") <- "show_all_licences"
  return(df)
}

#' @rdname show_all
#' @export show_all_reasons
show_all_reasons <- function() {
  
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_reasons")
  atlas <- getOption("galah_config")$atlas
  
  if(update_needed){ # i.e. we'd like to run a query
    ## return list of valid "reasons for use" codes
    url <- atlas_url("logger_reasons")
    out <- atlas_GET(url)
    if(is.null(out)){
      df <- galah_internal_cache()$show_all_reasons
      # if cached values reflect the correct atlas, return requested info
      if(attr(df, "atlas_name") == atlas){ 
        df <- galah_internal_cache()$show_all_reasons
        attr(df, "ARCHIVED") <- NULL # remove identifying attributes
        return(df)
        # otherwise return a message
      }else{ 
        bullets <- c(
          "Calling the API failed for `show_all_reasons`.",
          i = "This might mean that the system is down."
        )
        inform(bullets)
        return(tibble())
      }
      # if the API call works
    }else{
      if (any(names(out) == "deprecated")) out <- out[!out$deprecated, ]
      out <- out[wanted_columns("reasons")]
      # sort by id to make it less confusing
      row.names(out) <- out$id
      df <- as_tibble(out[order(out$id), ])
      attr(df, "atlas_name") <- atlas
      attr(df, "call") <- "show_all_reasons"
      return(df)
    }
    # if no update is needed
  }else{
    attr(df, "call") <- "show_all_reasons"
    return(galah_internal_cache()$show_all_reasons)
  }
}


#' @rdname show_all
#' @export show_all_ranks
show_all_ranks <- function() {
  if(getOption("galah_config")$atlas == "Global"){
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
  return(df)
}


#' @rdname show_all
#' @export show_all_profiles
show_all_profiles <- function() {
  
  # check whether the cache has been updated this session
  update_needed <- internal_cache_update_needed("show_all_profiles")
  
  if(update_needed){ # i.e. we'd like to run a query
    # return only enabled profiles?
    url <- atlas_url("profiles_all")
    resp <- atlas_GET(url)
    if(is.null(resp)){ # if calling the API fails, return cached data
      df <- galah_internal_cache()$show_all_profiles
      attr(df, "ARCHIVED") <- NULL # remove identifying attributes
    }else{
      df <- as_tibble(resp[wanted_columns(type = "profile")])
      galah_internal_cache(show_all_profiles = df)
    }    
  }else{
    df <- galah_internal_cache()$show_all_profiles
  }
  attr(df, "call") <- "show_all_profiles"
  df
}


#' @rdname show_all
#' @export show_all_lists
show_all_lists <- function(){
  url <- atlas_url("lists_all") |> 
    paste0(c(
      "?max=1000&offset=0", 
      "?max=1000&offset=1000", 
      "?max=1000&offset=2000")) 
  df <- do.call(rbind, 
          lapply(url, function(a){atlas_GET(a)$lists})) |> 
    tibble()
  attr(df, "call") <- "show_all_lists"
  return(df)
}