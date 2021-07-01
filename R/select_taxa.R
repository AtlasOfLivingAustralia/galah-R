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
#' taxanomic identifiers. If greater control is required to disambiguate search
#' terms, taxonomic levels can be provided explicitly via a named \code{list}
#' for a single name, or a \code{data.frame} for multiple names (see examples).
#' Note that searches are not case-sensitive.
#' @param children \code{logical}: return child concepts for the provided
#' query?
#' @param counts \code{logical}: return occurrence counts for all taxa
#' found? \code{FALSE} by default
#' @param all_ranks \code{logical}: include all available intermediate ranks for
#' taxa? e.g. suborder, superfamily. Retrieving this information requires an 
#' additional web service call so will slow down the query 
#' @return A \code{data.frame} of taxonomic information.
#' @seealso \code{\link{select_columns}}, \code{\link{select_filters}} and
#' \code{\link{select_locations}} for other ways to restrict the information returned
#' by \code{\link{ala_occurrences}} and related functions.
#' @examples
#' \dontrun{
#' # Search using a single term
#' select_taxa("Reptilia")
#' # or equivalently:
#' select_taxa("reptilia") # not case sensitive
#'
#' # Search with multiple ranks. This is required if a single term is a homonym.
#' select_taxa(
#'   list(kingdom = "Plantae", genus = "Microseris"),
#'   children = TRUE,
#'   counts = TRUE)
#'
#' # As above, but for multiple searches at once.
#' select_taxa(
#'    data.frame(
#'      genus = c("microseris", "Eucalyptus"),
#'      kingdom = "plantae"))
#'
#' # Search using an unique taxon identifier
#' select_taxa(query = "https://id.biodiversity.org.au/node/apni/2914510")
#'
#' # Search multiple taxa
#' select_taxa(c("reptilia", "mammalia")) # returns one row per taxon
#' }
#' @export select_taxa

select_taxa <- function(query, children = FALSE, counts = FALSE,
                        all_ranks = FALSE) {
  verbose <- ala_config()$verbose
  assert_that(is.flag(children))

  if (getOption("galah_config")$atlas != "Australia") {
    stop("`select_taxa` only provides information on Australian taxonomy. To search taxonomy for ",
         getOption("galah_config")$atlas, " use `taxize`. See vignette('international_atlases') for more information")
  }

  if (missing(query)) {
    stop("`select_taxa` requires a query to search for")
  }

  query_type <- deduce_query_type(query)

  if (verbose) {
    print_qt <- ifelse(query_type == "id", "identifiers",
                       "scientific or common names")
    message("Assuming that query term(s) provided are ", print_qt)
  }

  # caching won't catch if query order is changed
  cache_file <- cache_filename(c(unlist(query),
                               ifelse(children, "children", ""),
                               ifelse(counts, "counts", "")),
                               ext = ".csv")
  caching <- getOption("galah_config")$caching
  if (caching && file.exists(cache_file)) {
    # use cached file
    return(read.csv(cache_file))
  }

  if (query_type == "name") {
    matches <- name_query(query)
  } else {
    matches <- id_query(query)
  }
  
  out_data <- as.data.frame(matches, stringsAsFactors = FALSE)
  if (ncol(out_data) > 1 && children) {
    # look up the child concepts for the identifier
    children <- data.table::rbindlist(
      lapply(out_data$taxon_concept_id, function(x) {
        child_concepts(x)
      }), fill = TRUE)
    # add children to df
    out_data <- data.table::rbindlist(list(out_data, children), fill = TRUE)
  }
  if (ncol(out_data) > 1 && counts) {
    # add counts to data
    counts <- unlist(lapply(out_data$taxon_concept_id, function(id) {
      record_count(list(fq = paste0("lsid:", id)))
    }))
    out_data <- cbind(out_data, count = counts)
  }
  if (ncol(out_data) > 1 && all_ranks) {
    im_ranks <- data.table::rbindlist(
      lapply(out_data$taxon_concept_id, function(id) {
        intermediate_ranks(id)
      }
    ), fill = TRUE)
    out_data <- cbind(out_data, im_ranks)
    # Todo: order columns correctly
  }
  # write out to csv
  if (caching) {
    write.csv(out_data, cache_file, row.names = FALSE)
  }
  class(out_data) <- append(class(out_data), "ala_id")
  out_data
}

intermediate_ranks <- function(id) {
  url <- server_config("species_base_url")
  resp <- ala_GET(url, path = paste0("ws/species/", id))
  classification <- data.frame(resp$classification)
  classification <- classification[names(classification) %in%
                                     wanted_columns("extended_taxa")]
  return(classification)
}

id_query <- function(query) {
  matches <- data.table::rbindlist(lapply(query, function(t) {
    identifier_lookup(t)
  }), fill = TRUE)
}

name_query <- function(query) {
  ranks <- names(query)
  # check ranks are valid if query type is name
  validate_rank(ranks)
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

name_lookup <- function(name) {
  url <- server_config("name_matching_base_url")
  if (is.null(names(name)) || isTRUE(names(name) == "")) {
    # search by scientific name
    path <- "api/search"
    query <- list(q = name[[1]])
  } else {
    # search by classification
    path <- "api/searchByClassification"
    # workaround for https://github.com/AtlasOfLivingAustralia/ala-namematching-service/issues
    family_i <- which("family" %in% names(name))
    if (length(family_i) > 0) {
      names(name)[family_i] <- "scientificName"
    }
    query <- as.list(name)
  }
  result <- ala_GET(url, path, query)

  if ("homonym" %in% result$issues) {
    warning("Homonym issue with ", name,
         ". Please also provide another rank to clarify.")
    return(as.data.frame(list(search_term = name), stringsAsFactors = FALSE))
  }  else if (isFALSE(result$success)) {
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
  result <- ala_GET(taxa_url, "/api/getByTaxonID", list(taxonID = identifier))
  if (isFALSE(result$success) && result$issues == "noMatch") {
    message("No match found for identifier ", identifier)
  }
  names(result) <- rename_columns(names(result), type = "taxa")
  result[names(result) %in% wanted_columns("taxa")]
}

deduce_query_type <- function(query) {
  if (is.data.frame(query)) {
    return("name")
  }
  # Possible query types are 'id' and 'name'
  if (
    # APNI
    any(str_detect(query, "https://id.biodiversity.org.au")) ||
    # AFD
    any(str_detect(query, "afd.taxon:")) ||
    # NZOE
    any(str_detect(query, "NZOR")) ||
    # Index Fungorum
    any(str_detect(query, "urn:lsid:indexfungorum")) ||
    # Catalogue of life
    any(str_detect(query, "CoL:")) ||
    # CAAB
    !is.na(suppressWarnings(any(as.integer(query)))) ||
    # Aus Fungi and CoL - check if any digits in string- possibly this check
    # would cover all id types
    any(grepl("\\d", query)) ||
    # ALA special case
    any(str_detect(query, "ALA_"))) {
    return("id")
  }
  # By default assume 'name' type
  return("name")
}

# make sure rank provided is in accepted list
validate_rank <- function(ranks) {
  valid_ranks <- c("kingdom", "phylum", "class", "order",
                   "family", "genus", "specificEpithet")

  invalid_ranks <- ranks[which(!(ranks %in% valid_ranks))]

  if (length(invalid_ranks) != 0) {
    stop("Invalid rank(s): ", paste(invalid_ranks, collapse = ", "),
         ". Valid ranks are: ", paste0(valid_ranks, collapse = ", "))
  }
}

child_concepts <- function(identifier) {
  url <- server_config("species_base_url")
  path <- paste0("ws/childConcepts/",
                 URLencode(as.character(identifier), reserved = TRUE))
  children <- ala_GET(url, path)
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
