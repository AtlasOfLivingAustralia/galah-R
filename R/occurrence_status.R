#' Return the status of occurrence downloads
#' @export
occurrence_status <- function() {

  result <- url_GET("https://api.test.ala.org.au/common/biocache/occurrences/offline/status")
  result                             
}