#' Search ALA spatial layers
#'
#' The ALA stores information on a wide range of contextual and environmental
#' layers. \code{find_layers()} returns a \code{data.frame} of all layers
#' stored by the ALA.
#' To return layer values with occurrence records, pass the required layer
#' names to \code{\link{select_columns}}. To filter occurrence records by a
#' layer value (for layers with categorical values), pass the layer name and
#' value to \code{\link{select_filters}}. 
#'
#' @return A \code{data.frame} of all spatial layers held by the ALA
#' @export find_layers
#' @seealso This function is used to pass valid arguments to
#' \code{\link{select_columns}()} and \code{\link{select_filters}()}.
#' Layers are the subset of fields that are spatially appended to each record
#' by the ALA; the full list is given by \code{\link{find_fields}()}.
#' To view valid values for a layer with categorical values, use
#' \code{\link{find_field_values}()}.
#' @details The resulting \code{data.frame} contains the following columns:
#' \itemize{
#'  \item{layer_id: A prefix ('el' for environmental layers and 'cl' for
#'  contextual layers) followed by an id number. This is the value that should
#'  be used when referring to a layer in another function.}
#'  \item{name: Descriptive name of the layer.}
#'  \item{source_link: Link to original data source (if available)}
#'  \item{description: Additional details about layer}
#' }
#' @examples
#' \dontrun{
#' # Find all precipitation-related layers
#' layers <- find_layers()
#' layers[grepl("Precipitation", layers$name),]
#' 
#' # Find the IBRA 7 layer
#' ibra_7_layer <- layers[grepl("IBRA 7 Regions", layers$name),]
#' # View values for IBRA 7 layer
#' find_field_values(ibra_7_layer$layer_id)
#' }

find_layers <- function() {
  # web service returns all layers so might as well do that
  url <- getOption("galah_server_config")$base_url_spatial
  result <- ala_GET(url, "ws/layers")
  layer_id <- mapply(build_layer_id, result$type, result$id,
                      USE.NAMES = FALSE)
  result <- cbind(layer_id, result)
  names(result) <- rename_columns(names(result), type = "layer")
  result <- result[wanted_columns("layer")]
  result
}

build_layer_id <- function(type, id) {
  if (type == "Environmental") {
    paste0('el', id)
  } else {
    paste0('cl', id)
  }
}