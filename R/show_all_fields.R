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
  url <- atlas_url("spatial_layers", quiet = TRUE)
  result <- atlas_GET(url)
  
  if(is.null(result)){
    NULL
  }else{
    if(all(c("type", "id") %in% names(result))){
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
    }else{
      NULL
    }
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
  
  ## Original code showed fields returned by `show_all_media`
  ## These can't be queried with `galah_filter` and have been replaced
  # fields <- data.frame(id = c("imageId", "height", "width", "tileZoomLevels",
  #                             "thumbHeight", "thumbWidth", "filesize", "mimetype",
  #                             "creator", "title", "description", "rights",
  #                             "rightsHolder", "license", "imageUrl", "thumbUrl",
  #                             "largeThumbUrl", "squareThumbUrl", "tilesUrlPattern"))
  data.frame(
    id = c("multimedia", "multimediaLicence", "images", "videos", "sounds"),
    description = "Media filter field",
    type = "media"
  )
}

all_fields <- function() {
  url <- atlas_url("records_fields")
  atlas_GET(url)
}

build_layer_id <- function(type, id) {
  if (type == "Environmental") {
    paste0("el", id)
  } else {
    paste0("cl", id)
  }
}