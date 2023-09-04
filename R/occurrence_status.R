#' Return the status of occurrence downloads
#' @export
occurrence_status <- function() {

  api_authenticate()
  result <- url_GET("https://api.test.ala.org.au/common/biocache/occurrences/offline/status")
  result                             
}