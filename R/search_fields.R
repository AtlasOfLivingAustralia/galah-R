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
#' or \code{all}.
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
  type = "all" # or one of "fields", "layers", "assertions"
){
  # ensure data can be queried
  df <- switch(type,
    "fields" = get_fields(),
    "layers" = get_layers(),
    "assertions" = get_assertions(),
    "all" = {
      fields <- get_fields()
      layers <- get_layers()
      ass <- get_assertions()
      data.table::rbindlist(list(fields[!(fields$id %in% layers$id), ],
                                 layers, ass), fill = TRUE)
    },
    stop("`type`` must be one of c('fields', 'layers', 'assertions', 'all')")
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

  # replace name with dwc term if it exists
  fields$name <- ifelse(!is.na(fields$dwcTerm), fields$dwcTerm, fields$name)

  names(fields) <- rename_columns(names(fields), type = "fields")
  fields <- fields[wanted_columns("fields")]
  fields$type <- "fields"

  fields
}

get_assertions <- function() {
  url <- getOption("galah_server_config")$base_url_biocache
  assertions <- ala_GET(url, path = "ws/assertions/codes")
  assertions$data_type <- "logical"
  names(assertions) <- rename_columns(names(assertions), type = "assertions")
  assertions <- assertions[wanted_columns("assertions")]
  assertions$type <- "assertions"
  assertions
}

get_layers <- function() {
  url <- getOption("galah_server_config")$base_url_spatial
  result <- ala_GET(url, "ws/layers")
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

# Function to convert darwin core field name to ALA field name, as currently
# required by biocache APIs
dwc_to_ala <- function(dwc_names) {
  fields <- all_fields()
  # get relevant cols
  vapply(dwc_names, function(n) {
    if (n == "scientificName") {
      return("taxon_name")
    } else if (n == "verbatimLatitude") {
      return("verbatim_latitude")
    } else if (n == "verbatimLongitude") {
      return("verbatim_longitude")
    } else if (n == "verbatimCoordinateSystem") {
      return("verbatim_coordinate_system")
    } else if (n %in% fields$dwcTerm) {
      return(fields[fields$dwcTerm == n & !is.na(fields$dwcTerm), ]$name)
    } else {
      return(n)
    }
  }, USE.NAMES = FALSE, FUN.VALUE = character(1))
}

all_fields <- function() {
  url <- getOption("galah_server_config")$base_url_biocache
  ala_GET(url, path = "ws/index/fields")
}



build_layer_id <- function(type, id) {
  if (type == "Environmental") {
    paste0("el", id)
  } else {
    paste0("cl", id)
  }
}
