# This function is used to authenticate the user and generate JWT token which needs to be passed in API request headers

#' @importFrom httr oauth2.0_token oauth_app oauth_endpoint POST content
#' @importFrom jose jwt_split

api_authenticate <- function() {

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
                client_id = "3fs881sn87m35th0m61q6kqra2",
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
            key = "3fs881sn87m35th0m61q6kqra2",
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