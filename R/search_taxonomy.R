#' Search taxonomic trees
#'
#' The ALA has its' own internal taxonomy that is derived from authoritative
#' sources. \code{search_taxonomy} provides a means to query and visualise 
#' that taxonomy, by showing which lower clades are contained within a specified taxon.
#' The inverse query - i.e. listing the higher clades to which a taxon belongs
#' - is provided by \code{\link{select_taxa}()}. 
#' 
#' @param query \code{string}: A vector containing one or more search terms,
#' given as strings. Search terms can be scientific or common names, or
#' taxonomic identifiers. If greater control is required to disambiguate search
#' terms, taxonomic levels can be provided explicitly via a named \code{list}
#' for a single name. See vignette(taxonomic_information for using search_taxonomy
#' for multiple clades)
#' Note that searches are not case-sensitive.
#' @param down_to \code{string}: A taxonomic rank to search down to. See
#' \code{\link{find_ranks}} for valid inputs.
#' @details The approach used by this function is recursive, meaning that it  
#' becomes slow for large queries such as  
#' \code{search_taxonomy("Animalia", down_to = "species")}. The resulting 
#' \code{data.frame} can be passed to \code{\link{select_taxa}} to extract
#' unique identifiers for the terminal nodes (rows), which is necessary to use later 
#' functions such as \code{\link{ala_counts}} or \code{\link{ala_occurrences}}.
#' @return a \code{data.frame} containing one row per terminal node, and one 
#' column for every available intermediate taxonomic level.
#' @seealso \code{\link{select_taxa}} to search for an individual clade; 
#' \code{\link{find_ranks}} for valid ranks used to specify the \code{down_to}
#' argument.
#' @examples
#' \dontrun{
#' search_taxonomy("Animalia", down_to = "class")
#' }
#' @export

search_taxonomy <- function(query, down_to = NULL){
  # assert_that(is.logical(include_id))
  if (getOption("galah_config")$atlas != "Australia") {
    stop("`search_taxonomy` only provides information on Australian taxonomy. To search taxonomy for ",
         getOption("galah_config")$atlas, " use `taxize`. See vignette('international_atlases') for more information")
  }
  
  if (missing(query)) {
    stop("`search_taxonomy` requires a query to search for")
  }
  
  match <- name_lookup(query)
  start_row <- match[,c("scientific_name", "rank", "taxon_concept_id")]
  names(start_row) <- c("name", "rank", "guid")
  
  if (!is.null(down_to)) {
    if(!any(find_ranks()$name == down_to)){
    # if (rank_index(down_to) == 100) {
      stop("`down_to` must be a valid taxonomic rank")
    }
    down_to <- tolower(down_to)
    id_df <- rbind(start_row, level_down(start_row, down_to))
    
    # filter to the `down_to` rank
    id_df <- id_df[id_df$rank == down_to,]
  } else {
    id_df <- start_row
  }
  
  # get the classification 
  taxon_info <- as.data.frame(data.table::rbindlist(
    lapply(id_df$guid, function(id) {
      lookup_taxon(id)
    }
    ), fill = TRUE))
  # if (!include_id) {
  taxon_info <- taxon_info[, -which(grepl("id", names(taxon_info)))]
  # }
  names(taxon_info) <- rename_columns(names(taxon_info), type = "taxa")
  out_df <- taxon_info[, -which(names(taxon_info) %in%
                                     c("guid","scientific_name"))]
  # reorder columns according to taxonomic rank
  out_df <- out_df[order(
    match(names(out_df), find_ranks()$name)
  )]
  # convert cells to normal case
  title_case_df(as.data.frame(out_df), exclude = "authority")
}

# Return the classification for a taxonomic id
lookup_taxon <- function(id) {
  url <- server_config("species_base_url")
  resp <- ala_GET(url, path = paste0("ws/species/", id))
  taxon_info <- resp$classification
  taxon_info$authority <- resp$taxonConcept$nameAuthority
  taxon_info$author <- resp$taxonConcept$author
  data.frame(taxon_info)
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
level_down <- function(taxon_row, down_to) {
  if (rank_index(taxon_row$rank) >= rank_index(down_to)) {
    return(taxon_row[,c("name", "rank", "guid")])
  }
  children <- get_children(taxon_row$guid)
  if (length(children) == 0 || nrow(children) == 0) {
    return(taxon_row[,c("name", "rank", "guid")])
  }
  data.table::rbindlist(lapply(seq_len(nrow(children)), function(i) {
    level_down(children[i,], down_to)
  }))
}
  
# Return the index of a taxonomic rank- lower index corresponds to higher up the
# tree
rank_index <- function(rank) {
  all_ranks <- find_ranks()
  if (rank %in% all_ranks$name) {
    return(all_ranks$id[all_ranks$name == rank])  
  }else{
    return(100)
  }
}
