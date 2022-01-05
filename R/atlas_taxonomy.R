#' Search taxonomic trees
#'
#' The ALA has its' own internal taxonomy that is derived from authoritative
#' sources. `atlas_taxonomy` provides a means to query 
#' that taxonomy, returning a tree (class `Node`) showing which lower 
#' clades are contained within the specified taxon.
#' 
#' @param taxa The identity of the clade for which a taxonomic
#' hierarchy should be returned. Should be specified using an object of class 
#' `data.frame` and `ala_id`, as returned from
#' [search_taxa()]. 
#' @param down_to The identity of the clade at which the downwards search
#' should stop. Should be specified using an object of class 
#' `character` and `galah_down_to`, as returned from
#' [galah_down_to()]. Also accepts a string.
#' @details The approach used by this function is recursive, meaning that it  
#' becomes slow for large queries such as  
#' `atlas_taxonomy(search_taxa("Plantae"), down_to = galah_down_to(species))`.
#' Although the inputs to `search_taxa` and `down_to` are 
#' case-insensitive, node names are always returned in title case.
#' @return A tree consisting of objects of class `Node`, containing the 
#' requested taxonomy. Each node contains the following attributes:
#' 
#'   * `name`: The scientific name of the taxon in question
#'   * `rank`: The taxonomic rank to which that taxon belongs
#'   * `guid`: A unique identifier used by the ALA
#'   * `authority`: The source of the taxonomic name & identifier
#' 
#' @seealso [search_taxa()] to search for an individual taxon; 
#' [show_all_ranks()] for valid ranks used to specify the `down_to`
#' argument.
#' @examples
#' atlas_taxonomy(search_taxa("chordata"), down_to = galah_down_to(class))
#' 
#' @export

atlas_taxonomy <- function(taxa, down_to){

  if (getOption("galah_config")$atlas != "Australia") {
    stop("`atlas_taxonomy` only provides information on Australian taxonomy. To search taxonomy for ",
      getOption("galah_config")$atlas, 
      " use `taxize`. See vignette('international_atlases') for more information")
  }
 
  # error checking for `taxa`
  if (missing(taxa)) {
    stop("argument `taxa` is missing, with no default")}
  
  if(!inherits(taxa, "ala_id")){
    stop("`atlas_taxonomy` requires an object of class `ala_id`; see `?search_taxa` for more information")}
  
  if(nrow(taxa) > 1){
    stop("`atlas_taxonomy` only accepts a single taxon at a time")
  }
  
  # error checking for `down_to`
  if (missing(down_to)) {
    stop("argument `down_to` is missing, with no default")
  }
  assert_that(is.string(down_to)) # picks up NULL etc
  down_to <- tolower(down_to) 
  if(!any(show_all_ranks()$name == down_to)){
    stop("`down_to` must be a valid taxonomic rank")
  }
  
  # extract required information from `taxa`
  start_row <- taxa[, c("scientific_name", "rank", "taxon_concept_id")]
  names(start_row) <- c("name", "rank", "guid")
  start_row$name <- str_to_title(start_row$name)
  
  # run a test to check whether the search will work
  test <- get_children(start_row$guid)
  if(is.null(test)){
    inform("Calling the API failed for `atlas_taxonomy`")
    id_tree <- Node$new(
      name = taxa$scientific_name,
      rank = taxa$rank,
      guid = taxa$taxon_concept_id
    )
  }else{
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
  }
  return(id_tree)    

}

# Return the classification for a taxonomic id
lookup_taxon <- function(id) {
  url <- server_config("species_base_url")
  resp <- atlas_GET(url, path = paste0("ws/species/", id))
  if(is.null(resp)){
    return(NULL)
  }else{
    taxon_info <- resp$classification
    taxon_info$authority <- resp$taxonConcept$nameAuthority
    taxon_info$author <- resp$taxonConcept$author
    return(data.frame(taxon_info))
  }
}

# Get the child concepts for a taxonomic ID 
get_children <- function(identifier) {
  url <- server_config("species_base_url")
  path <- paste0(
    "ws/childConcepts/",
    URLencode(as.character(identifier), reserved = TRUE)
  )
  return(atlas_GET(url, path))
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
  all_ranks <- show_all_ranks()
  if (rank %in% all_ranks$name) {
    return(all_ranks$id[all_ranks$name == rank])  
  }else{
    return(100)
  }
}
