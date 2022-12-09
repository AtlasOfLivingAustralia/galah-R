#' (currently internal) function to create 'predicates'
#' @importFrom jsonlite toJSON unbox

# JSON for passing to GBIF offline downloads API.
# https://www.gbif.org/developer/occurrence

# note: GBIF requires a password as well as a username;
# might be worth considering what system ALA will require soon,
# and integrating both approaches.

build_predicates <- function(df){ # where df is returned by galah_filter()
  
  send_notification <- getOption("galah_config")$package$send_email |> 
    as.character() |> 
    tolower()

  data_list <- list(
    creator = getOption("galah_config")$user$username,
    notificationAddresses = getOption("galah_config")$user$email,
    sendNotification = send_notification,
    format = "SIMPLE_CSV",
    predicate = list(
      type = "and",
      predicates = parse_predicates(df)
    )
  )
  
  toJSON(data_list)
}

# parse galah_filter result into predicate format
# test object:
# df <- galah_filter(year == 2010, basisOfRecord == "PRESERVED_SPECIMEN")
parse_predicates <- function(df){
  lapply(
    split(df, seq_len(nrow(df))),
    function(a){
      list(
        type = unbox(switch(a$logical,
                      "==" = "equals",
                      "<" = "lessThan",
                      "<=" = "lessThanOrEquals",
                      ">" = "greaterThan",
                      ">=" = "greaterThanOrEquals",
                      "!=" = "not"
                      )),
        key = unbox(a$variable),
        value = unbox(a$value)
      )
    })
}
# NOTE: currently missing: `within` (geolocate), `or`, `in`, `isNull`, `isNotNull`


