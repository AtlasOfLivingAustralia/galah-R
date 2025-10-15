#' Return the status of occurrence downloads
#' @export
occurrence_status <- function() {

   api_authenticate()
   tokens <- read_csv("./data-raw/tokens.csv")
   access_token <- tokens$value[which(tokens$token == "access_token")[1]]
   apikey <- tokens$value[which(tokens$token == "apikey")[1]]

   string1 <- "Bearer"
   string2 <- access_token
   auth_header <- paste(string1, string2)

  result <- url_GET("https://api.test.ala.org.au/common/biocache/occurrences/offline/status",
  add_headers(
      Authorization = auth_header,
      "x-api-key" = apikey,
      Accept = "application/json"
    )
  )
  print(result)
  result                             
}