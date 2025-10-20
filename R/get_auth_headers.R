#' @importFrom httr oauth2.0_token oauth_app oauth_endpoint POST content
#' @importFrom jose jwt_split

library(httr2)
library(jsonlite)
library(jose)
library(R6)

if (!exists(".session", envir = .GlobalEnv)) {
  .session <<- new.env(parent = emptyenv())
}

# Function to generate ALA API R authentication headers
get_auth_headers <- function() {
  # Step 1: Retrieve access_token from session, or call another function to get it
  access_token <- get_access_token_from_session()  # Replace with your session retrieval function
  if (is.null(access_token) || access_token == "") {
    access_token <- retrieve_new_access_token()  # Fetch a fresh token
  } else if (as.numeric(Sys.time()) > jwt_split(access_token)$payload$exp) {
    access_token <- refresh_access_token()  # Refresh expired token
  }

  # Step 2: Retrieve API Gateway key from session or configuration
  apikey <- get_apikey_from_session()  # Replace with your session/config retrieval
  if (is.null(apikey) || apikey == "") {
    apikey <- retrieve_new_apikey()  # Fetch API key
  }

  # Step 3: Construct the Authorization header
  header <- paste("Bearer", access_token)

  # Step 4: Create and return the headers as a named list
  headers <- list(
    "User-Agent" = galah_version_string(),
    "Authorization" = header,
    "x-api-key" = apikey
  )

  return(headers)
}

# Placeholder functions
get_access_token_from_session <- function() {
 if (exists(".session", envir = .GlobalEnv) && !is.null(.session$access_token)) {
     return(.session$access_token)
   } else {
     return(NULL)
   }
 }
get_refresh_token_from_session <- function() {
  if (exists(".session", envir = .GlobalEnv) && !is.null(.session$refresh_token)) {
      return(.session$refresh_token)
    } else {
      return(NULL)
    }
 }
get_apikey_from_session <- function() {
  if (exists(".session", envir = .GlobalEnv) && !is.null(.session$apikey)) {
       return(.session$apikey)
     } else {
       return(NULL)
     }
  }
galah_version_string <- function() { "Galah-R/1.0" }

# Retrieve new access token using PKCE
retrieve_new_access_token <- function() {
  auth_config <- retrieve_auth_config()

  client <- oauth_client(
    id = auth_config$client_id,
    token_url = auth_config$token_url
  )

  # Create an OAuth2 PKCE flow
  token <- oauth_flow_auth_code(
    client = client,
    auth_url = auth_config$authorize_url,
    pkce = TRUE,
    scope = paste(unlist(strsplit(auth_config$scopes, " ")), collapse = " "),
    redirect_uri = "http://localhost:1410/"
  )

  # Extract access token
  access_token <- token$access_token
  #Save access_token, refresh token in session
  .session$access_token <- access_token
  .session$refresh_token <- token$refresh_token
  return(access_token)
}

# Refresh expired access token
refresh_access_token <- function() {
  auth_config <- retrieve_auth_config()  # Make sure auth_config is available here
  refresh_token <- get_refresh_token_from_session()

  refresh_url <- auth_config$token_url
  req_params <- list(
    refresh_token = refresh_token,
    client_id = auth_config$client_id,
    grant_type = "refresh_token",
    client_secret = NULL
  )

  response <- POST(refresh_url, body = req_params, encode = "form")
  refresh_data <- content(response)
  access_token <- refresh_data$access_token

  #Save access_token in session
  .session$access_token <- access_token
  return(access_token)
}

# Retrieve new API key
retrieve_new_apikey <- function() {
  # Ensure 'header' is available or replace with valid token
  header <- paste("Bearer", get_access_token_from_session())

  response <- GET(
    url = "https://api.test.ala.org.au/common/api/getApikey",
    add_headers(
      "User-Agent" = galah_version_string(),
      "Authorization" = header
    )
  )

  apikey <- content(response, as = "text", encoding = "UTF-8")
  print(apikey)
  #Save apikey in session
  .session$apikey <- apikey
  return(apikey)
}

# Retrieve authentication configuration
retrieve_auth_config <- function() {
  url <- "https://api.test.ala.org.au/common/api/getAuthConfig"
  config <- fromJSON(url)

  auth_config <- list(
    client_id     = config$client_id,
    authorize_url = config$authorize_url,
    token_url     = config$token_url,
    scopes        = config$scopes
  )

  return(auth_config)
}