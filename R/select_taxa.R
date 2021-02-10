#' Taxon information
#'
#' The resulting dataframe of taxon information can be passed directly to
#' \code{\link{ala_occurrences}} to filter records to the specified taxa.
#'
#' @param term string: named list or dataframe: search term(s)
#' @param term_type string: specifies which type of terms are provided in
#' `term`. One of name \code{`c('name', 'identifier')`}. Default behaviour is
#' to search by `name`. `identifier` refers to the unique identifier for a
#' taxon.
#' @param return_children logical: return child concepts for the provided
#' term(s)?
#' @param include_counts logical: return occurrence counts for all taxa
#' found? \code{FALSE} by default
#' @return data.frame of taxon information
#' @examples
#' \dontrun{
#' # Search using a single term
#' select_taxa(term = "Reptilia")
#'
#' # Search with multiple ranks. This is required if a single term is a homonym.
#' select_taxa(list(kingdom = "Plantae", genus = "Microseris"),
#'          return_children = TRUE, include_counts = TRUE)
#'
#' # Search using an unique taxon identifier
#' select_taxa(term = "https://id.biodiversity.org.au/node/apni/2914510",
#'          term_type = "identifier")
#' }
#' @export select_taxa

select_taxa <- function(term, term_type = "name", return_children = FALSE,
                         include_counts = FALSE) {

  assert_that(is.flag(return_children))
  assert_that(term_type %in% c("name", "identifier"),
              msg = "`term_type` must be one of `c('name', 'identifier')`")

  if (missing(term)) {
    stop("`select_taxa` requires a term to search for")
  }
  # caching won't catch if term order is changed
  cache_file <- cache_filename(c(unlist(term), term_type,
                               ifelse(return_children, "children", ""),
                               ifelse(include_counts, "counts", "")),
                               ext = ".csv")
  caching <- getOption("galah_config")$caching
  if (caching && file.exists(cache_file)) {
    # use cached file
    return(read.csv(cache_file))
  }

  if (term_type == "name") {
    ranks <- names(term)
    # check ranks are valid if term type is name
    validate_rank(ranks)
    if (is.list(term) && length(names(term)) > 0 ) {
      # convert to dataframe for simplicity
      term <- as.data.frame(term)
    }
    if (is.data.frame(term)) {
      matches <- data.table::rbindlist(apply(term, 1, name_lookup),
                                       fill = TRUE)
    } else {
      matches <- data.table::rbindlist(lapply(term, function(t) {
        name_lookup(t)
      }), fill = TRUE)
    }
  } else {
    matches <- data.table::rbindlist(lapply(term, function(t) {
      identifier_lookup(t)
    }), fill = TRUE)
  }
  out_data <- as.data.frame(matches, stringsAsFactors = FALSE)
  if (ncol(out_data) > 1 && return_children) {
    # look up the child concepts for the identifier
    children <- data.table::rbindlist(
      lapply(out_data$taxon_concept_id, function(x) {
        child_concepts(x)
      }))
    # add children to df
    out_data <- data.table::rbindlist(list(out_data, children), fill = TRUE)
  }
  if (ncol(out_data) > 1 && include_counts) {
    counts <- unlist(lapply(out_data$taxon_concept_id, function(id) {
      record_count(list(fq = paste0("lsid:", id)))
    }))
    out_data <- cbind(out_data, count = counts)
  }
  # write out to csv
  if (caching) {
    write.csv(out_data, cache_file)
  }
  out_data
}


name_lookup <- function(name) {
  url <- getOption("galah_server_config")$base_url_name_matching
  if (is.null(names(name)) || names(name) == "") {
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
  taxa_url <- getOption("galah_server_config")$base_url_name_matching
  result <- ala_GET(taxa_url, "/api/getByTaxonID", list(taxonID = identifier))
  if (isFALSE(result$success) && result$issues == "noMatch") {
    message("No match found for identifier ", identifier)
  }
  names(result) <- rename_columns(names(result), type = "taxa")
  result[names(result) %in% wanted_columns("taxa")]
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
  url <- getOption("galah_server_config")$base_url_bie
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
