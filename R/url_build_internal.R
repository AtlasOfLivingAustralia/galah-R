#' build URLs for LAs
#' @param .data an object of class `data_query`, created by 
#' `collapse.data_request()`. Formerly `build_fq_url()`
#' @keywords Internal
#' @importFrom httr build_url
#' @importFrom httr parse_url
#' @noRd
url_build_internal <- function(.data){
  url <- parse_url(.data$url)
  params <- .data$query
  if(any(names(params) == "fq")){
    # join_char <- ifelse(length(url$query) > 0, "&fq=", "?fq=")
    
    # ensure all arguments from galah_filter are enclosed in brackets
    fq <- params$fq
    missing_brackets <- !grepl("^\\(", fq)
    if(any(missing_brackets)){
      fq[missing_brackets] <- paste0("(", fq[missing_brackets], ")")
    }
    fq_single <- paste(fq, collapse = "AND")
    url$query <- c(fq = fq_single, params[names(params) != "fq"])
  }
  return(build_url(url))
}