#' @name show_all_minifunctions
#' @rdname show_all_minifunctions
#' @title Show all valid options for various types of information
#' @description 
#' The living atlases store a huge amount of information, above and beyond the 
#' occurrence records that are their main output. In `galah`, one way that 
#' users can investigate this information is by showing all the available 
#' options or categories for the type of information they are interested in. 
#' Functions prefixed with `show_all_` do this, displaying all valid options 
#' for the information specified by the suffix. 
#' 
#' @details There are five categories of information, each with their own 
#' sub-functions, or "minifunctions" for to display each type. 
#' The available `show_all_` sub-functions are:
#' 
#' | **Category** | **Sub-function** | **Description** |
#' |---|---|---|
#' | configuration  |`show_all_atlases()`| Show what atlases are available |
#' | |`show_all_apis()`| Show what APIs & functions are available for each atlas |
#' | |`show_all_reasons()`| Show what values are acceptable as 'download reasons' for a specified atlas |
#' | taxonomy |`show_all_ranks()`| Show valid taxonomic ranks (e.g. Kingdom, Class, Order, etc.) |
#' | filters |`show_all_fields()`| Show fields that are stored in an atlas |
#' | |`show_all_assertions()`| Show results of data quality checks run by each atlas |
#' | |`show_all_licences()`| Show what copyright licences are applied to media |
#' | group filters|`show_all_profiles()`| Show what data profiles are available |
#' | |`show_all_lists()`| Show what species lists are available|
#' | data providers|`show_all_providers()`| Show which institutions have provided data |
#' | |`show_all_collections()`|Show the specific collections within those institutions|
#' | |`show_all_datasets()`|Shows all the data groupings within those collections|
#' 
#' @aliases show_all_assertions show_all_atlases show_all_cached_files 
#' @aliases show_all_collections show_all_datasets show_all_providers 
#' @aliases show_all_fields show_all_reasons show_all_ranks show_all_profiles 
#' @aliases show_all_values show_all_licences
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble)
#' @seealso [show_all()] and [search_all()] are flexible helper functions to 
#' show and/or search for information returned by each look-up sub-function.
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' See all supported atlases
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_atlases()
#' ```
#'
#' See a listing of all valid fields and layers
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_fields()
#' ```
#'
#' Show a listing of all accepted reasons for downloading occurrence data
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_reasons()
#' ```
#' 
#' Add your download reason when configuring your session with [galah_config()]
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(download_reason_id = 3)
#' ```
#'
#' Show a list of all available data quality profiles
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_profiles()
#' ```
#' 
#' Values in the `shortName` column can be used with [galah_filter()] to 
#' narrow your data query results
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_filter(profile == "ALA")
#' ```
#' Show a listing of all taxonomic ranks
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_ranks()
#' ```
#' 
#' Use ranks with [galah_down_to()] and [atlas_taxonomy()] to get taxonomic 
#' trees
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' galah_call() %>%
#'   galah_identify("fungi") %>%
#'   galah_down_to(subphylum) %>%
#'   atlas_taxonomy()
#' ```
#' 
#' 
#' Configure caching and create a query to cache with [galah_config()]
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' galah_config(caching = TRUE)
#' dat <- atlas_counts(group_by = galah_group_by(year))
#' ```
#'
#' Show a listing of previously cached files
#' 
#' ```{r, comment = "#>", collapse = TRUE, eval = FALSE}
#' show_all_cached_files()
#' ```
#' 
NULL

#' @rdname show_all_minifunctions
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
#' @rdname show_all_minifunctions
show_all_atlases <- function() {node_metadata}


#' @rdname show_all_minifunctions
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


#' @rdname show_all_minifunctions
#' @export show_all_collections
show_all_collections <- function(){
  url <- atlas_url("collections_collections")
  atlas_GET(url) |> tibble()
}


#' @rdname show_all_minifunctions
#' @export show_all_datasets
show_all_datasets <- function(){
  url <- atlas_url("collections_datasets")
  atlas_GET(url) |> tibble()
}


#' @rdname show_all_minifunctions
#' @export show_all_providers
show_all_providers <- function(){
  url <- atlas_url("collections_providers")
  atlas_GET(url) |> tibble()
}


#' @rdname show_all_minifunctions
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
      return(df)
    }
    
    # if no update needed
  }else{    
    df <- galah_internal_cache()$show_all_fields
    return(df)
  }   
}

#' @rdname show_all_minifunctions
#' @export show_all_licences
show_all_licences <- function(){
  url <- atlas_url("image_licences")
  result <- atlas_GET(url) |> tibble()
  result[, c("id", "name", "acronym", "url")] 
}

#' @rdname show_all_minifunctions
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
      return(df)
    }
    # if no update is needed
  }else{
    return(galah_internal_cache()$show_all_reasons)
  }
}


#' @rdname show_all_minifunctions
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
  return(df)
}


#' @rdname show_all_minifunctions
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
  df
}