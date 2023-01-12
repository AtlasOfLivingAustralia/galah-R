#' @importFrom jsonlite toJSON unbox
NULL

# predicates are JSON scripts for passing to GBIF offline downloads API.
# https://www.gbif.org/developer/occurrence

build_predicates <- function(
  df, # where df is returned by galah_filter()
  format = "SIMPLE_CSV"
){ 

  if(nrow(df) < 1){
    return(NULL)
  }

  send_notification <- getOption("galah_config")$package$send_email |>
    as.character() |>
    tolower()

  predicates_list <- parse_predicates(df)
  if(nrow(df) > 1){
     predicates_list <- list(
      type = unbox("and"),
      predicates = parse_predicates(df)
    )
  }else{
     predicates_list <- parse_predicates(df)[[1]]
  }

  data_list <- list(
    creator = unbox(getOption("galah_config")$user$username),
    notificationAddresses = getOption("galah_config")$user$email,
    sendNotification = unbox(send_notification),
    format = unbox(format),
    predicate = predicates_list
  )

  toJSON(data_list)
}

# parse galah_filter result into predicate format
parse_predicates <- function(df){
  json_text <- lapply(
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
        key = unbox(gbif_upper_case(a$variable)),
        value = unbox(a$value)
      ) 
    })
    names(json_text) <- NULL
    return(json_text)
}
# NOTE: currently missing: `within` (geolocate), `or`, `in`, `isNull`, `isNotNull`
# Q: how to add taxonomic names to gbif schema?

# test object:
# df <- galah_filter(year == 1850)
# df <- galah_filter(catalogNumber == 217880)

gbif_upper_case <- function(string){
  gsub("(?=[[:upper:]])", "_", string, perl = TRUE) |> toupper()
}