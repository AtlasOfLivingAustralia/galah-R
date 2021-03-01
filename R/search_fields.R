#' Query layers or fields by free text search
#'
#' Function to search the ALA database for layers or fields
#' using a free-text search.
#'
#' @param query \code{string}: A search string. Not case sensitive.
#' @param type \code{string}: What type of parameters should be searched?
#' Should be one of \code{fields}, \code{layers} or \code{both}.
#' @return A \code{data.frame} with three columns:
#' \itemize{
#'  \item{id: The identifier for that layer or field. This is the value that should
#'  be used when referring to a field in another function.}
#'  \item{description: Detailed information on a given field}
#'  \item{type: Whether the field is a \code{field} or \code{layer}}
#' }
#' @export search_fields
#' @examples
#' \dontrun{
#' test <- search_fields("species")
#' }

search_fields <- function(
  query,
  type = "both" # or "fields" or "layers"
){
  # ensure data can be queried
  df <- switch(type,
    "fields" = get_standard_fields(),
    "layers" = get_standard_layers(),
    "both" = {
      fields <- get_standard_fields()
      layers <- get_standard_layers()
      rbind(fields[!(fields$id %in% layers$id), ], layers)
    }
  )

  # merge info together into searchable strings
  df_string <- tolower(
    apply(df[, 1:2], 1, function(a){paste(a, collapse = " ")}))

  # run a query
  return(df[grepl(tolower(query), df_string), ])

}


# internal functions
get_standard_fields <- function(){
  result <- find_fields()[, c("name", "info")]
  colnames(result) <- c("id", "description")
  result$type = "fields"
  return(result)
}

get_standard_layers <- function(){
  result <- find_layers()
  result$description <- apply(
    result[, c("name", "description")],
    1,
    function(a){paste(a, collapse = " ")}
  )
  result <- result[, c("layer_id", "description")]
  colnames(result)[1] <- "id"
  result$type <- "layers"
  return(result)
}
