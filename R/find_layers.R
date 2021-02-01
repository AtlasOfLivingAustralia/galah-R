#' Search ALA spatial layers
#' 
#' To return layer values with occurrence records, pass the required layer
#' names to \code{\link{select_columns}}.
#' 
#' @return data.frame of all spatial layers held by the ALA
#' @export find_layers
#' @examples
#' \dontrun{
#' # Find all precipitation-related layers
#' layers <- find_layers()
#' layers[grepl("Precipitation", layers$name),]
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