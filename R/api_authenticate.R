#' Internal function to authenticate the user and generate JWT token 
#' 
#' Tokens are passed in API request headers. This function was written by 
#' Yasima 08-2023 and amended by MW. Note that the required APIs are not yet in 
#' production, and 'final' (lol) architecture of the function has not been
#' decided.
#' 
#' The original version of this code used `{httr}`, but `{galah}` uses 
#' `{httr2}`. The syntax of these two packages is not identical, so some testing
#' will be needed to ensure the translation is correct. The below is based on
#' https://httr2.r-lib.org/articles/oauth.html
#' 
# @importFrom httr2 oauth_client 
# @importFrom httr2 oauth_flow_auth_code
#' @noRd
#' @keywords Internal
api_authenticate <- function() {
  
  # build a client
  client <- oauth_client(
    id = "galah",
    token_url = "https://auth-secure.auth.ap-southeast-2.amazoncognito.com/oauth2/token",
    key = "hs79ejhce04n1vms1kju7ejqf",
    auth = "header")
  
  # get a token
  token <- oauth_flow_auth_code(
    client = app,
    auth_url = "https://auth-secure.auth.ap-southeast-2.amazoncognito.com/oauth2/authorize",
    scope = c("openid","profile","email", "ala/attrs", "ala/roles"))

  # use token to get API key
  result <- collect_api_keys(app, token)
  
  # unclear what next. Save out? Switch APIs?
  result
}

#' Experimental internal function for collecting API key from new API
#' 
#' NOTE: this code is basically borrowed from 
#' https://httr2.r-lib.org/articles/oauth.html - a better choice might be to 
#' integrate with `query_API()`.
#' @importFrom httr2 request
#' @importFrom httr2 req_headers
# @importFrom httr2 req_oauth_auth_code
#' @importFrom httr2 req_perform
#' @importFrom httr2 resp_body_json
#' @noRd
#' @keywords Internal
collect_api_keys <- function(client, token){
  request(url_lookup("api_key"))|>
    req_headers(build_headers(jwt = TRUE))|>
    req_oauth_auth_code(
      client = client, 
      auth_url = "https://auth-secure.auth.ap-southeast-2.amazoncognito.com/oauth2/authorize") |> 
    req_perform() |> 
    resp_body_json()
}

#' Original code provided by Yasima for JWT token generation. 
#' 
#' This function is used to authenticate the user and generate JWT token which 
#' needs to be passed in API request headers
# @importFrom httr oauth2.0_token oauth_app oauth_endpoint POST content
#' @importFrom jose jwt_split
#' @noRd
#' @keywords Internal
api_authenticate_original <- function() {
  
  # this assumes that a CSV with tokens have already been generated and stored 
  # locally
  tokens <- read_csv("./data-raw/tokens.csv")
  access_token <- tokens$value[which(tokens$token == "access_token")[1]]
  apikey <- tokens$value[which(tokens$token == "apikey")[1]]
  refresh_token <- tokens$value[which(tokens$token == "refresh_token")[1]]

  if(!is.null(access_token)) {
      tokenData <- jwt_split(access_token)
      if(as.numeric(as.POSIXct(Sys.time())) > tokenData$payload$exp){
            refresh_url <- "https://auth-secure.auth.ap-southeast-2.amazoncognito.com/oauth2/token"
            req_params <- list(
              refresh_token = refresh_token,
              client_id = "5kahrda00sbg0g64su20d8ebkt",
              grant_type = "refresh_token",
              client_secret = NULL
            )
            response <- POST(refresh_url, body = req_params, encode = "form")
            refresh_data <- content(response)
            access_token <- refresh_data$access_token
      }
  }

  if(is.null(access_token)){
      endpoint <- oauth_endpoint(
          authorize = "https://auth-secure.auth.ap-southeast-2.amazoncognito.com/oauth2/authorize",
          access = "https://auth-secure.auth.ap-southeast-2.amazoncognito.com/oauth2/token"
      )
      app <- oauth_app(
          "galah",
          key = "5kahrda00sbg0g64su20d8ebkt",
          secret = NULL
      )

      key_response <- oauth2.0_token(
          endpoint,
          app,
          scope = c("openid","profile","email", "ala/attrs", "ala/roles"),
          type = "application/json",
          use_basic_auth = FALSE,
          config_init = list(),
          client_credentials = FALSE,
          credentials = NULL,
          as_header = TRUE
      )

      access_token <- key_response$credentials$access_token
      refresh_token <-  key_response$credentials$refresh_token
  }

  string1 <- "Bearer"
  string2 <- access_token
  header <- paste(string1, string2)
  
  cli <- HttpClient$new(
    url = "https://api.test.ala.org.au/common/api/getApikey",
    headers = list("User-Agent" = galah_version_string(), "Authorization" = header))
  
  response <- try(cli$get(), silent = TRUE)
  apikey <- response$parse("UTF-8")
  
  fileConn<-file("./data-raw/tokens.csv")
  writeLines(c("token,value", paste("apikey,",apikey), paste("access_token,",access_token), paste("refresh_token,",refresh_token)), fileConn)
  close(fileConn)
}