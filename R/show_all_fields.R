#' Query layers, fields or assertions by free text search
#'
#' This function can be used to find relevant fields and/or layers
#' for use in building a set of filters with [galah_filter()] or
#' specifying required columns with [galah_select()].
#' This function returns a `data.frame` of all fields matching the type
#' specified.
#' Field names are in Darwin Core format, except in the case where the field is
#' specific to the ALA database, in which case the ALA field name is returned.
#'
#' @references 
#' *  Darwin Core terms <https://dwc.tdwg.org/terms/>
#' *  ALA fields <https://api.ala.org.au/#ws72>
#' *  ALA assertions fields <https://api.ala.org.au/#ws81>
#' 
#' @param type `string`: What type of parameters should be searched?
#' Should be one or more of `fields`, `layers`, `assertions`,
#' `media` or `all`.
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) with 
#' three columns:
#' 
#'  * id: The identifier for that layer or field. This is the value that should
#'  be used when referring to a field in another function.
#'  * description: Detailed information on a given field
#'  * type: Whether the field is a `field` or `layer`
#'  * link: For layers, a link to the source data (if available)
#' 
#' @seealso This function is used to pass valid arguments to
#' [galah_select()] and [galah_filter()].
#' To view valid values for a layer with categorical values, use
#' [search_field_values()].
#'
#' @details
#' Layers are the subset of fields that are spatially appended to each record
#' by the ALA. Layer ids are comprised of a prefix: 'el' for environmental
#' (gridded) layers and 'cl' for contextual (polygon) layers,  followed by an
#' id number.
#' 
#' @section Examples:
#' ```{r, child = "man/rmd/setup.Rmd"}
#' ```
#' 
#' See a listing of all valid fields and layers
#' 
#' ```{r, comment = "#>", collapse = TRUE}
#' show_all_fields()
#' ```
#' 
#' Use this function with [search_fields()] to find fields and layers you wish 
#' to use to filter your data queries
#' 
#' 
#' @export show_all_fields

show_all_fields <- function(
  type = c("all", "fields", "layers", "assertions", "media", "other")
){
  
  # check type is valid, and return an error if it is not
  type <- match.arg(type)

  # check whether the cache has been updated this session
  atlas <- getOption("galah_config")$atlas
  update_needed <- internal_cache_update_needed("show_all_fields")
 
  if(update_needed){ # i.e. we'd like to run a query
    df <- switch(type,
      "fields" = get_fields(),
      "layers" = get_layers(),
      "assertions" = get_assertions(),
      "media" = get_media(),
      "other" = get_other_fields(),
      "all" = {
        fields <- get_fields()
        layers <- get_layers()
        assertions <- get_assertions()
        media <- get_media()
        other <- get_other_fields()
        if(any(
          is.null(fields),
          is.null(layers),
          is.null(assertions),
          # is.null(media) # internally generated
          is.null(other)
        )){
          NULL
        }else{
          as.data.frame(
            rbindlist(
              list(
                fields[!(fields$id %in% layers$id), ],
                layers, assertions, media, other), 
              fill = TRUE)
          )
        }
      }
    )
    
    # if calling the API fails
    if(is.null(df)){ 
      df <- galah_internal_cache()$show_all_fields
      # if cached values reflect the correct atlas, return requested info
      if(attr(df, "atlas_name") == atlas){ 
        attr(df, "ARCHIVED") <- NULL # remove identifying attributes
        if(type == "all"){
          return(df)
        }else{
          return(df[df$type == type, ])
        }
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
      # and the request was for `type = 'all'`, then update cache
      if(type == "all"){
        df <- as_tibble(df)
        attr(df, "atlas_name") <- atlas
        galah_internal_cache(show_all_fields = df)
        return(df)
      # otherwise just return required df
      }else{
        df <- as_tibble(df)
        attr(df, "atlas_name") <- atlas
        return(df)       
      }
    }
  }else{ # i.e. no update needed    
    df <- galah_internal_cache()$show_all_fields
    if(type == "all"){
      return(df)
    }else{
      return(df[df$type == type, ])
    }
  }   
}

# Helper functions to get different field classes
get_fields <- function() {
  fields <- all_fields()
  if(is.null(fields)){
    NULL
  }else{
    # remove fields where class is contextual or environmental
    fields <- fields[!(fields$classs %in% c("Contextual", "Environmental")),]

    names(fields) <- rename_columns(names(fields), type = "fields")
    fields <- fields[wanted_columns("fields")]
    fields$type <- "fields"

    fields
  }
}

get_assertions <- function() {
  url <- server_config("records_base_url")
  assertions <- atlas_GET(url, path = "assertions/codes")
  if(is.null(assertions)){
    NULL
  }else{
    assertions$data_type <- "logical"
    names(assertions) <- rename_columns(names(assertions), type = "assertions")
    assertions <- assertions[wanted_columns("assertions")]
    assertions$type <- "assertions"
    assertions
  }
}

get_layers <- function() {
  url <- server_config("spatial_base_url")
  result <- atlas_GET(url, "layers")
  if(is.null(result)){
    NULL
  }else{
    layer_id <- mapply(build_layer_id, result$type, result$id,
                       USE.NAMES = FALSE)
    result <- cbind(layer_id, result)
    result$description <- apply(
      result[, c("displayname", "description")],
      1,
      function(a){paste(a, collapse = " ")}
    )
    names(result) <- rename_columns(names(result), type = "layer")
    result <- result[wanted_columns("layer")]
    names(result)[1] <- "id"
    result$type <- "layers"
    result
  }
}

# Return fields not returned by the API
get_other_fields <- function() {
  data.frame(id = "qid", description = "Reference to pre-generated query",
             type = "other")
}

# There is no API call to get these fields, so for now they are manually
# specified
get_media <- function(x) {
  fields <- data.frame(id = c("imageId", "height", "width", "tileZoomLevels",
                              "thumbHeight", "thumbWidth", "filesize", "mimetype",
                              "creator", "title", "description", "rights",
                              "rightsHolder", "license", "imageUrl", "thumbUrl",
                              "largeThumbUrl", "squareThumbUrl", "tilesUrlPattern"))
  fields$description <- "Media filter field"
  fields$type <- "media"
  fields
}

all_fields <- function() {
  url <- server_config("records_base_url")
  atlas_GET(url, path = "index/fields")
}

build_layer_id <- function(type, id) {
  if (type == "Environmental") {
    paste0("el", id)
  } else {
    paste0("cl", id)
  }
}
