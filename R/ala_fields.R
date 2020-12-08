#' Retrieves a list of all field names that can be used with data
#' retrieval functions
#'
#' The field names are in Darwin Core format, apart from the fields
#' which have no DwC equivalent, such as record assertions.
#'
#' @references Relevant ALA web services: \itemize{
#' \item for occurrence fields: \url{https://api.ala.org.au/#ws72}
#' \item for assertions \url{https://api.ala.org.au/#ws81}
#' }
#' @seealso \code{\link{ala_layers}} to search for spatial layers
#' @param class string: class of fields to return e.g. "Assertion"
#' @return data.frame of all fields. 
#' @examples
#' \dontrun{
#'  l <- ala_fields()
#' }
#' @export ala_fields

ala_fields <- function(class = "all") {
  # Difference in behaviour from original ALA fields: 
  # don't need to return layer information- this is handled by `ala_layers`
  # assertions and other fields are treated the same- but the type for assertions is 'logical'
  # if there is a DwC term for a field, this will be returned: not the ALA name
  
  # To do: check if a user has provided params/ used `field_info` and warn if so
  
  # for backwards compatibility, should allow a user to get the ALA names for a field?

  url <- getOption("koala_server_config")$base_url_biocache
  
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
dwc_to_ala <- function(dwc_names) {
  fields <- all_fields()
  # get relevant cols
  vapply(dwc_names, function(n) {
    if (n %in% fields$dwcTerm) {
      return(fields[fields$dwcTerm == n & !is.na(fields$dwcTerm), ]$name)
    } else {
      return(n)
    }
  }, USE.NAMES = FALSE, FUN.VALUE = character(1))
}

all_fields <- function() {
  url <- getOption("koala_server_config")$base_url_biocache
  ala_GET(url, path = "ws/index/fields")
}
