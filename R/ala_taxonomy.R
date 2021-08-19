#' Search taxonomic trees
#'
#' The ALA has its' own internal taxonomy that is derived from authoritative
#' sources. \code{search_taxonomy} provides a means to query 
#' that taxonomy, returning a tree (class \code{Node}) showing which lower 
#' clades are contained within the specified taxon.
#' 
#' @param taxa The identity of the clade for which a taxonomic
#' hierarchy should be returned. Should be specified using an object of class 
#' \code{data.frame} and \code{ala_id}, as returned from
#' \code{\link{select_taxa}()}. 
#' @param down_to \code{string}: A taxonomic rank to search down to. See
#' \code{\link{find_ranks}()} for valid inputs.
#' @details The approach used by this function is recursive, meaning that it  
#' becomes slow for large queries such as  
#' \code{search_taxonomy(select_taxa("Plantae"), down_to = "species")}.
#' Although the inputs to \code{select_taxa} and \code{down_to} are 
#' case-insensitive, node names are always returned in title case.
#' @return A tree consisting of objects of class \code{Node}, containing the 
#' requested taxonomy. Each node contains the following attributes:
#' \itemize{
#'   \item\code{name}: The scientific name of the taxon in question
#'   \item\code{rank}: The taxonomic rank to which that taxon belongs
#'   \item\code{guid}: A unique identifier used by the ALA
#'   \item\code{authority}: The source of the taxonomic name & identifier
#' }
#' @seealso \code{\link{select_taxa}} to search for an individual taxon; 
#' \code{\link{find_ranks}} for valid ranks used to specify the \code{down_to}
#' argument.
#' @examples
#' \dontrun{
#' search_taxonomy(select_taxa("chordata"), down_to = "class")
#' }
#' @export

ala_taxonomy <- function(taxa, down_to){

  if (getOption("galah_config")$atlas != "Australia") {
    stop("`search_taxonomy` only provides information on Australian taxonomy. To search taxonomy for ",
      getOption("galah_config")$atlas, 
      " use `taxize`. See vignette('international_atlases') for more information")
  }
 
  # error checking for `taxa`
  if (missing(taxa)) {
    stop("argument `taxa` is missing, with no default")}
  
  if(!inherits(taxa, "ala_id")){
    stop("`ala_taxonomy` requires an object of class `ala_id`; see `?select_taxa` for more information")}
  
  if(nrow(taxa) > 1){
    stop("`ala_taxonomy` only accepts a single taxon at a time")
  }
  
  # error checking for `down_to`
  if (missing(down_to)) {
    stop("argument `down_to` is missing, with no default")}
    
  assert_that(is.string(down_to))
  
  # extract required information from `taxa`
  start_row <- taxa[, c("scientific_name", "rank", "taxon_concept_id")]
  names(start_row) <- c("name", "rank", "guid")
  start_row$name <- str_to_title(start_row$name)
  
  if (!is.null(down_to)) {
    if(!any(find_ranks()$name == down_to)){
      stop("`down_to` must be a valid taxonomic rank")
    }
    down_to <- tolower(down_to)   
    id_list <- level_down(start_row, down_to) # run recursive queries
    id_tree <- FromListExplicit(id_list) # convert to data.tree
    
    # calculate which branches end in down_to,
    # and remove those that don't
    id_tree$Do(
      function(a){a$rank_value <- as.numeric(a$rank == down_to)}) 
    id_tree$Do(
      function(a){a$rank_value <- Aggregate(
        node = a, attribute = "rank_value", aggFun = sum)},
      traversal = "post-order")
    invisible(Prune(id_tree, pruneFun = function(a){a$rank_value > 0}))
    id_tree$Set(rank_value = NULL) # remove column used for calculations
    
    # get authority/source
    id_tree$Set(authority = unlist(lapply(
      ToDataFrameTree(id_tree, "guid")$guid,
      function(id){lookup_taxon(id)$authority})))
  
    return(id_tree)    
  }
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

# Take a taxon row and recurse down the taxonomic tree 
# until the provided rank is reached
level_down <- function(taxon_row, down_to) {
  if(!(taxon_row$rank %in% c("unranked", "informal"))){
    if (rank_index(taxon_row$rank) >= rank_index(down_to)) {
      result <- taxon_row[c("name", "rank", "guid")]
      result$name <- str_to_title(result$name)
      return(result)
    }
  }
  children <- get_children(taxon_row$guid)
  valid_children <- !grepl("[[:space:]]+", children$name)
  if(length(which(valid_children)) < 1){
    result <- taxon_row[c("name", "rank", "guid")]
    result$name <- str_to_title(result$name)
    return(result)
  }else{
    children <- children[valid_children, ]
    children$name <- str_to_title(children$name)
    next_list <- lapply(
      seq_len(nrow(children)), 
      function(i) {level_down(children[i,], down_to)})
    return( 
      append(
        as.list(taxon_row[c("name", "rank", "guid")]), 
        list(children = next_list)))
  }
}

# Return the index of a taxonomic rank- 
# lower index corresponds to higher up the tree
rank_index <- function(rank) {
  all_ranks <- find_ranks()
  if (rank %in% all_ranks$name) {
    return(all_ranks$id[all_ranks$name == rank])  
  }else{
    return(100)
  }
}
