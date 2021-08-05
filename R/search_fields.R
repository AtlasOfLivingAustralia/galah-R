#' Query layers, fields or assertions by free text search
#'
#' This function can be used to find relevant fields and/or layers
#' for use in building a set of filters with \code{\link{select_filters}()} or
#' specifying required columns with \code{\link{select_columns}()}.
#' This function returns a \code{data.frame} of all fields matching the type
#' specified.
#' Field names are in Darwin Core format, except in the case where the field is
#' specific to the ALA database, in which case the ALA field name is returned.
#'
#' @references \itemize{
#' \item Darwin Core terms \url{https://dwc.tdwg.org/terms/}
#' \item ALA fields \url{https://api.ala.org.au/#ws72}
#' \item ALA assertions fields \url{https://api.ala.org.au/#ws81}
#' }
#' @param query \code{string}: A search string. Not case sensitive.
#' @param type \code{string}: What type of parameters should be searched?
#' Should be one or more of \code{fields}, \code{layers}, \code{assertions},
#' \code{media} or \code{all}.
#' @return A \code{data.frame} with three columns:
#' \itemize{
#'  \item{id: The identifier for that layer or field. This is the value that should
#'  be used when referring to a field in another function.}
#'  \item{description: Detailed information on a given field}
#'  \item{type: Whether the field is a \code{field} or \code{layer}}
#'  \item{link: For layers, a link to the source data (if available)}
#' }
#' @seealso This function is used to pass valid arguments to
#' \code{\link{select_columns}()} and \code{\link{select_filters}()}.
#' To view valid values for a layer with categorical values, use
#' \code{\link{find_field_values}()}.
#' @export search_fields
#'
#' @details
#' Layers are the subset of fields that are spatially appended to each record
#' by the ALA. Layer ids are comprised of a prefix: 'el' for environmental
#' (gridded) layers and 'cl' for contextual (polygon) layers,  followed by an
#' id number.
#' @examples
#' \dontrun{
#' test <- search_fields("species")
#'
#' # Find all WorldClim layers
#' worldclim <- search_fields("worldclim", type = "layers")
#'
#' # Return a data.frame containing all data on fields and layers
#' all_fields <- search_fields()
#' }

search_fields <- function(
  query,
  type = c("all", "fields", "layers", "assertions", "media", "other")
){
  type <- match.arg(type)
  # ensure data can be queried
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
      result <- as.data.frame(
        data.table::rbindlist(
          list(
            fields[!(fields$id %in% layers$id), ],
            layers, assertions, media, other), 
          fill = TRUE)
      )
    },
    stop("`type`` must be one of c('fields', 'layers', 'assertions','other', 'all')")
  )

  # merge info together into searchable strings
  df_string <- tolower(
    apply(df[, 1:2], 1, function(a){paste(a, collapse = " ")}))

  if (missing(query) || is.null(query)) {
    return(df)
  }
  # run a query
  return(df[grepl(tolower(query), df_string), ])

}

# Helper functions to get different field classes
get_fields <- function() {
  fields <- all_fields()
  # remove fields where class is contextual or environmental
  fields <- fields[!(fields$classs %in% c("Contextual", "Environmental")),]

  names(fields) <- rename_columns(names(fields), type = "fields")
  fields <- fields[wanted_columns("fields")]
  fields$type <- "fields"

  fields
}

get_assertions <- function() {
  url <- server_config("records_base_url")
  assertions <- ala_GET(url, path = "assertions/codes")
  assertions$data_type <- "logical"
  names(assertions) <- rename_columns(names(assertions), type = "assertions")
  assertions <- assertions[wanted_columns("assertions")]
  assertions$type <- "assertions"
  assertions
}

get_layers <- function() {
  url <- server_config("spatial_base_url")
  result <- ala_GET(url, "layers")
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
  ala_GET(url, path = "index/fields")
}

build_layer_id <- function(type, id) {
  if (type == "Environmental") {
    paste0("el", id)
  } else {
    paste0("cl", id)
  }
}
