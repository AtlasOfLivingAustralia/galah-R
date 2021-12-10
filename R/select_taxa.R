#' Taxon information
#'
#' In the ALA, all records are associated with an identifier that uniquely
#' identifies the taxon to which that record belongs. However, taxonomic names
#' are often ambiguous due to homonymy; i.e. re-use of names (common or
#' scientific) in different clades. Hence, \code{select_taxa} provides a means
#' to search for taxonomic names and check the results are 'correct' before
#' proceeded to download data via \code{\link{ala_occurrences}()},
#' \code{\link{ala_species}()} or \code{\link{ala_counts}()}. The resulting
#' \code{data.frame} of taxonomic information can be passed directly to
#' \code{ala_} functions to filter records to the specified taxon or taxa.
#'
#' @param query \code{string}: A vector containing one or more search terms,
#' given as strings. Search terms can be scientific or common names, or
#' taxonomic identifiers. If greater control is required to disambiguate search
#' terms, taxonomic levels can be provided explicitly via a named \code{list}
#' for a single name, or a \code{data.frame} for multiple names (see examples).
#' Note that searches are not case-sensitive.
#' @param is_id \code{logical}: Is the query a unique identifier? Defaults to 
#' \code{FALSE}, meaning that queries are assumed to be taxonomic names.
#' @return An object of class \code{data.frame} and \code{ala_id}
#' containing taxonomic information.
#' @seealso \code{\link{select_columns}}, \code{\link{galah_filter}} and
#' \code{\link{galah_location}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions.
#' \code{\link{ala_taxonomy}} to look up taxonomic trees.
#' @examples
#' \dontrun{
#' # Search using a single term
#' select_taxa("Reptilia")
#' # or equivalently:
#' select_taxa("reptilia") # not case sensitive
#'
#' # Search using an unique taxon identifier
#' select_taxa(query = "https://id.biodiversity.org.au/node/apni/2914510")
#'
#' # Search multiple taxa
#' select_taxa(c("reptilia", "mammalia")) # returns one row per taxon
#' }
#' @export
select_taxa <- function(...) {
  UseMethod("select_taxa")
}

#' @export
#' @rdname select_taxa
select_taxa.data_request <- function(request, ...) {
  request$taxa <- do.call(select_taxa, merge_args(request, list(...)))
  return(request)
}

#' @export
#' @rdname select_taxa
select_taxa.default <- function(query, is_id = FALSE) {
  assert_that(is.logical(is_id))
  if(missing(is_id)){is_id <- FALSE}
  verbose <- getOption("galah_config")$verbose

  if (getOption("galah_config")$atlas != "Australia") {
    stop("`select_taxa` only provides information on Australian taxonomy. To search taxonomy for ",
         getOption("galah_config")$atlas, " use `taxize`. See vignette('international_atlases') for more information")
  }

  if (missing(query)) {
    stop("`select_taxa` requires a query to search for")
  }
  
  if (is_id) {
    matches <- id_query(query)   
  } else {
    query <- stringr::str_remove_all(query, "[()]") # remove parentheses
    matches <- name_query(query)
  }
  
  out_data <- as.data.frame(matches, stringsAsFactors = FALSE)
  class(out_data) <- append(class(out_data), "ala_id")
  out_data
}

id_query <- function(query) {
  matches <- data.table::rbindlist(lapply(query, function(t) {
    identifier_lookup(t)
  }), fill = TRUE)
}

name_query <- function(query) {
  if (is.list(query) && length(names(query)) > 0 ) {
    # convert to dataframe for simplicity
    query <- as.data.frame(query)
  }
  if (is.data.frame(query)) {
    matches <- data.table::rbindlist(apply(query, 1, name_lookup),
                                     fill = TRUE)
  } else {
    matches <- data.table::rbindlist(lapply(query, function(t) {
      name_lookup(t)
    }), fill = TRUE)
  }
  return(matches)
}

intermediate_ranks <- function(id) {
  url <- server_config("species_base_url")
  resp <- atlas_GET(url, path = paste0("ws/species/", id))
  classification <- data.frame(resp$classification)
  classification <- classification[names(classification) %in%
                                     wanted_columns("extended_taxa")]
  return(classification)
}

name_lookup <- function(name) {
  url <- server_config("name_matching_base_url")
  if (is.null(names(name)) || isTRUE(names(name) == "")) {
    # search by scientific name
    path <- "api/search"
    query <- list(q = name[[1]])
  } else {
    # search by classification
    path <- "api/searchByClassification"
    name <- validate_rank(name)
    query <- as.list(name)
  }
  result <- atlas_GET(url, path, query)

  if ("homonym" %in% result$issues) {
    warning("Homonym issue with ", name,
         ". Please also provide another rank to clarify.")
    # return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
  }  #else 
  if (isFALSE(result$success)) {
    message("No taxon matches were found for \"", name, "\"")
    return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
  }
  names(result) <- rename_columns(names(result), type = "taxa")

  # if search term includes more than one rank, how to include in output?
  if (length(name) > 1) {
    name <- paste(unname(unlist(name)), collapse  = "_")
  }
  cbind(search_term = name,
        as.data.frame(result[names(result) %in% wanted_columns("taxa")],
                      stringsAsFactors = FALSE))
}

identifier_lookup <- function(identifier) {
  taxa_url <- server_config("name_matching_base_url")
  result <- atlas_GET(taxa_url, "/api/getByTaxonID", list(taxonID = identifier))
  if (isFALSE(result$success) && result$issues == "noMatch") {
    message("No match found for identifier ", identifier)
  }
  names(result) <- rename_columns(names(result), type = "taxa")
  result[names(result) %in% wanted_columns("taxa")]
}


# make sure rank provided is in accepted list
validate_rank <- function(df) {
   
  ranks <- names(df)
  ranks_check <- ranks %in% find_ranks()$name
  if(any(ranks_check)){
    return(df[ranks_check])
  }else{
    return(NULL)
  }

}

child_concepts <- function(identifier) {
  url <- server_config("species_base_url")
  path <- paste0("ws/childConcepts/",
                 URLencode(as.character(identifier), reserved = TRUE))
  children <- atlas_GET(url, path)
  if (length(children) == 0) {
    message("No child concepts found for taxon id \"", identifier, "\"")
    return()
  }

  # lookup details for each child concept
  child_info <- suppressWarnings(data.table::rbindlist(lapply(children$guid,
                                                              function(id) {
    result <- identifier_lookup(id)
    # keep child even if it can"t be found?
    names(result) <- rename_columns(names(result), type = "taxa")
    result <- result[names(result) %in% wanted_columns("taxa")]
    as.data.frame(result, stringsAsFactors = FALSE)
  }), fill = TRUE))
  child_info
}
