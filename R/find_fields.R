#' Retrieves field names for use with data retrieval functions
#'
#' When building a set of filters with \code{\link{select_filters}()} or
#' specifying required columns with \code{\link{select_columns}()}, this function
#' can be used to check that the fields map to fields stored in the ALA.
#' This function returns a \code{data.frame} of all fields. Field names are in
#' Darwin Core format, except in the case where the field is specific to the
#' ALA database, in which case the ALA field name is returned.
#'
#' @references \itemize{
#' \item Darwin Core terms \url{https://dwc.tdwg.org/terms/}
#' \item ALA fields \url{https://api.ala.org.au/#ws72}
#' \item ALA assertion fields \url{https://api.ala.org.au/#ws81}
#' }
#' @param class \code{string}: class of fields to return e.g. "Assertion". By
#' default all fields are returned.
#' @return \code{data.frame} of fields with name, data_type, information and Darwin
#' Core class.
#' @seealso This function is used to pass valid arguments to
#' \code{\link{select_filters}()} & \code{\link{select_columns}()}. The information
#' that this function gives on spatial layers is not particularly detailed;
#' consider also checking \code{\link{find_layers}()} for more information.
#' @details The resulting \code{data.frame} contains the following columns:
#' \itemize{
#'  \item{name: Basic description of the field. This is the value that should
#'  be used when referring to a field in another function.}
#'  \item{data_type: equivalent to the 'class' of a variable in R}
#'  \item{info: Additional details about layer}
#'  \item{class: Info on the group to which each field belongs}
#' }
#' @examples
#' \dontrun{
#' # Find all fields
#'  fields <- find_fields()
#'  # Find assertion fields
#'  assertions <- find_fields("assertion")
#' }
#' @export find_fields

find_fields <- function(class = "all") {
  # Difference in behaviour from original ALA fields:
  # don't need to return layer information- this is handled by `find_layers`
  # assertions and other fields are treated the same- but the type for assertions is 'logical'
  # if there is a DwC term for a field, this will be returned: not the ALA name

  # for backwards compatibility, should allow a user to get the ALA names for a field?

  url <- getOption("galah_server_config")$base_url_biocache

  fields <- ala_GET(url, path = "ws/index/fields")

  # replace name with dwc term if it exists
  fields$name <- ifelse(!is.na(fields$dwcTerm), fields$dwcTerm, fields$name)

  names(fields) <- rename_columns(names(fields), type = "fields")
  fields <- fields[wanted_columns("fields")]

  # add assertions
  assertions <- ala_GET(url, path = "ws/assertions/codes")
  assertions$data_type <- "logical"
  assertions$class <- "Assertion"
  names(assertions) <- rename_columns(names(assertions), type = "assertions")
  assertions <- assertions[wanted_columns("assertions")]
  all_fields <- rbind(fields, assertions)
  if (class == "all") {
    return(all_fields)
  }
  all_fields[tolower(all_fields$class) == tolower(class) &
               !is.na(all_fields$class),]
}


# function to keep backwards compatibility
# takes field list and converts back to ALA name
# TODO: Fix for scientific names which map to multiple ALA names
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
