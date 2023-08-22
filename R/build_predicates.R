#' Build predicates
#' 
#' predicates are JSON scripts for passing to GBIF offline downloads API.
#' https://www.gbif.org/developer/occurrence
#' @noRd
#' @keywords Internal
#' @importFrom potions pour
#' @importFrom jsonlite toJSON 
#' @importFrom jsonlite unbox
build_predicates <- function(
  df, # where df is returned by galah_filter()
  format = "SIMPLE_CSV"
){ 

  if(nrow(df) < 1){
    return(NULL)
  }

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
    creator = unbox(pour("user", "username", .pkg = "galah")),
    notificationAddresses = pour("user", "email", .pkg = "galah"),
    sendNotification = unbox(pour("package", "send_email", .pkg = "galah")),
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