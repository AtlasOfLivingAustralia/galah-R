#' Build predicates
#' 
#' predicates are JSON scripts for passing to GBIF offline downloads API.
#' https://www.gbif.org/developer/occurrence
#' @importFrom potions pour
#' @importFrom jsonlite toJSON 
#' @importFrom jsonlite unbox
#' @noRd
#' @keywords Internal
build_predicates <- function(
  df, # where df is returned by galah_filter()
  location,
  format = "SIMPLE_CSV"
){ 
  if(nrow(df) < 1){
    return(NULL)
  }

  predicates_list <- c(
    parse_predicates(df),
    parse_predicates_spatial(location))
  
  if(length(predicates_list) > 1){
     predicates_list <- list(
      type = unbox("and"),
      predicates = predicates_list
    )
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

#' most code borrowed from `build_query_gbif()`
#' @noRd
#' @keywords Internal
parse_predicates_spatial <- function(location){
  if(!is.null(location)) {
    # if location is for a point radius vs polygon/bbox
    if(!is.null(names(location))){
      if(all(!is.null(location$radius))) { # `galah_radius()` will always pass radius argument
        list(type = unbox("geoDistance"),
             latitude = unbox(location$lat),
             longitude = unbox(location$lon),
             distance = unbox(paste0(location$radius, "km"))) |>
          list()
      }else{
        list(type = unbox("within"), 
             geometry = unbox(location)) |>
          list()
      }
    }else{
      list(type = unbox("within"), 
           geometry = unbox(location)) |>
        list()
    }
  }else{
    NULL
  }
}

#' parse galah_filter result into predicate format
#' @importFrom jsonlite unbox
#' @noRd
#' @keywords Internal
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
# NOTE: currently missing: `or`, `in`, `isNull`, `isNotNull`

# test object:
# df <- galah_filter(year == 1850)
# df <- galah_filter(catalogNumber == 217880)

gbif_upper_case <- function(string){
  gsub("(?=[[:upper:]])", "_", string, perl = TRUE) |> toupper()
}