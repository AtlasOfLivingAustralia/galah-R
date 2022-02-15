#' @rdname show_all_minifunctions
#' @export show_all_fields

show_all_fields <- function(){

  # check whether the cache has been updated this session
  atlas <- getOption("galah_config")$atlas
  update_needed <- internal_cache_update_needed("show_all_fields")
 
  if(update_needed){ # i.e. we'd like to run a query
    fields <- get_fields()
    layers <- get_layers()
    media <- get_media()
    other <- get_other_fields()
    if(any(
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
