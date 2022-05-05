#' @export show_all_assertions
#' @rdname show_all_minifunctions
show_all_assertions <- function(){
  get_assertions() |> as_tibble()
}

#' @rdname search_minifunctions
#' @export search_assertions
search_assertions <- function(query){
  df <- show_all_assertions()
  df[grepl(tolower(query), tolower(df$description)), ]
}


# internal function
get_assertions <- function() {
  if(getOption("galah_config")$atlas == "Global"){
    gbif_assertions()
  }else{
    url <- server_config("records_base_url")
    assertions <- atlas_GET(url, path = "assertions/codes")
    if(is.null(assertions)){
      NULL
    }else{
      assertions$data_type <- "logical"
      names(assertions) <- rename_columns(names(assertions), type = "assertions")
      assertions <- assertions[wanted_columns("assertions")]
      assertions$type <- "assertions"
      assertions
    }
  }
}

# https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
gbif_assertions <- function() {
  tibble(
    id = c(
      "AMBIGUOUS_COLLECTION",
      "AMBIGUOUS_INSTITUTION",
      "BASIS_OF_RECORD_INVALID"
    ),
    description = c(
      "The given collection matches with more than 1 GRSciColl collection",
      "The given institution matches with more than 1 GRSciColl institution",
      "The given basis of record is impossible to interpret or significantly different from the recommended vocabulary"
    ),
    type = "assertions"
  )
}