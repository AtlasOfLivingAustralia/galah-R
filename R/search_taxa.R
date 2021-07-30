#' Search taxon information
#'
#' In the ALA, all records are associated with an identifier that uniquely
#' identifies the taxon to which that record belongs. However, taxonomic names
#' can be ambiguous due to homonymy; i.e. re-use of names (common or
#' scientific) in different clades. Hence, \code{search_taxa} provides a means
#' to search for taxonomic names and check the results are 'correct' before
#' proceeded to download data via \code{\link{ala_occurrences}()},
#' \code{\link{ala_species}()} or \code{\link{ala_counts}()}.
#' 
#' @param query \code{string}: A vector containing one or more search terms,
#' given as strings. Search terms can be scientific or common names, or
#' taxanomic identifiers. If greater control is required to disambiguate search
#' terms, taxonomic levels can be provided explicitly via a named \code{list}
#' for a single name. See vignette(taxonomic_information for using search_taxa
#' for multiple taxons)
#' Note that searches are not case-sensitive.
#' @param downto \code{string}: A taxonomic rank to search down to
#' @param include_ids \code{logical} Include unique taxonomic identifiers for
#' all of the ranks? \code{FALSE} by default.
#' @export

search_taxa <- function(query, downto = NULL, include_ids = FALSE){
  assert_that(is.logical(include_ids))
  if (getOption("galah_config")$atlas != "Australia") {
    stop("`search_taxa` only provides information on Australian taxonomy. To search taxonomy for ",
         getOption("galah_config")$atlas, " use `taxize`. See vignette('international_atlases') for more information")
  }
  
  if (missing(query)) {
    stop("`search_taxa` requires a query to search for")
  }
  
  match <- name_lookup(query)
  start_row <- match[,c("scientific_name", "rank", "taxon_concept_id")]
  names(start_row) <- c("name", "rank", "guid")
  
  
  if (!is.null(downto)) {
    if (rank_index(downto) == 100) {
      stop("`downto` must be a valid taxonomic rank")
    }
    downto <- tolower(downto)
    id_df <- rbind(start_row, level_down(start_row, downto))
    
    # filter to the `downto` rank
    id_df <- id_df[id_df$rank == downto,]
  } else {
    id_df <- start_row
  }
  
  # get the classification 
  classified_df <- as.data.frame(data.table::rbindlist(
    lapply(id_df$guid, function(id) {
      classification(id)
    }
    ), fill = TRUE))
  if (!include_ids) {
    classified_df <- classified_df[, -which(grepl("id", names(classified_df)))]
  }
  # convert to normal case
  names(classified_df) <- rename_columns(names(classified_df), type = "taxa")
  out_df <- classified_df[, -which(names(classified_df) %in%
                                     c("guid","scientific_name"))]
  title_case_df(as.data.frame(out_df))
}

# Return the classification for a taxonomic id
# optionally include ids
classification <- function(id) {
  url <- server_config("species_base_url")
  resp <- ala_GET(url, path = paste0("ws/species/", id))
  data.frame(resp$classification)
}

# Return the index of a taxonomic rank- lower index corresponds to higher up the
# tree
rank_index <- function(rank) {
  ranks_list <- c("root", "superkingdom", "kingdom", "subkingdom", 
                  "superphylum", "phylum", "subphylum", "superclass", "class", 
                  "subclass", "infraclass", "subinfraclass", 
                  "superdivison zoology", "division zoology", 
                  "subdivision zoology", "supercohort", "cohort", "subcohort", 
                  "superorder", "order", "suborder", "infraorder", "parvorder", 
                  "superseries zoology", "series zoology", "subseries zoology", 
                  "supersection zoology", "section zoology", 
                  "subsection zoology", "superfamily", "family", "subfamily", 
                  "infrafamily", "supertribe", "tribe", "subtribe", 
                  "supergenus", "genus group", "genus", "nothogenus", 
                  "subgenus", "supersection botany", "section botany", 
                  "subsection botany", "superseries botany", "series botany", 
                  "subseries botany", "species group", "superspecies", 
                  "species subgroup", "species", "nothospecies", "holomorph", 
                  "anamorph", "teleomorph", "subspecies", "nothosubspecies", 
                  "infraspecies", "variety", "nothovariety", "subvariety", 
                  "form", "nothoform", "subform", "biovar", "serovar", 
                  "cultivar", "pathovar", "infraspecific")
  if (!rank %in% ranks_list) {
    return(100)
  }
  return(which(ranks_list == rank))
}

# Get the child concepts for a taxonomic ID 
get_children <- function(identifier) {
  url <- server_config("species_base_url")
  path <- paste0(
    "ws/childConcepts/",
    URLencode(as.character(identifier), reserved = TRUE)
  )
  return(ala_GET(url, path))
}

# Take a taxon row and recurse down the taxonomic tree until the provided rank
# is reached
level_down <- function(taxon_row, downto) {
  if (rank_index(taxon_row$rank) >= rank_index(downto)) {
    return(taxon_row[,c("name", "rank", "guid")])
  }
  children <- get_children(taxon_row$guid)
  if (length(children) == 0 || nrow(children) == 0) {
    return(taxon_row[,c("name", "rank", "guid")])
  }
  data.table::rbindlist(lapply(seq_len(nrow(children)), function(i) {
    level_down(children[i,], downto)
  }))
}
  