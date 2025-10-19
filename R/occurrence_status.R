#' Return the status of occurrence downloads
#' @export
occurrence_status <- function() {

   headers = get_auth_headers()
   result <- url_GET("https://api.test.ala.org.au/common/biocache/occurrences/offline/status",
      do.call(add_headers, headers)
   )
  print(result)
  result                             
}