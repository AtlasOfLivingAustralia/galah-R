#' Search taxon information
#' 
#' @export



# if (is_higher_rank(current_rank, downto)) {
#   lower_ranks <- 
# }

search_taxa <- function(query, downto = NULL){
  if (getOption("galah_config")$atlas != "Australia") {
    stop("`search_taxa` only provides information on Australian taxonomy. To search taxonomy for ",
         getOption("galah_config")$atlas, " use `taxize`. See vignette('international_atlases') for more information")
  }
  
  if (missing(query)) {
    stop("`search_taxa` requires a query to search for")
  }
  
  matches <- name_query(query)
  out_data <- as.data.frame(matches, stringsAsFactors = FALSE)
  
  if (!is.null(downto)) {
    taxon_row <- data.frame(name = out_data$scientific_name,
                      rank = out_data$rank,
                      guid = out_data$taxon_concept_id)
    out_data <- rbind(taxon_row, level_down(taxon_row, downto))
    
    # filter to the ranks we want
    out_data <- out_data[out_data$rank == downto,]
  }
  
  all_ranks <- data.table::rbindlist(
    lapply(out_data$guid, function(id) {
      intermediate_ranks(id)
    }
    ), fill = TRUE)
  out_data <- cbind(out_data, all_ranks)
    
  out_data
}

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
  if (rank == "unranked") {
    return(100)
  }
  return(which(ranks_list == rank))
}

get_children <- function(identifier) {
  url <- server_config("species_base_url")
  path <- paste0(
    "ws/childConcepts/",
    URLencode(as.character(identifier), reserved = TRUE)
  )
  return(ala_GET(url, path))
}

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
  
  
  

  # lookup details for each child concept
  # child_info <- suppressWarnings(data.table::rbindlist(lapply(
  #   children$guid,
  #   function(id) {
  #     result <- identifier_lookup(id)
  #     # keep child even if it can"t be found?
  #     names(result) <- rename_columns(names(result), type = "taxa")
  #     result <- result[names(result) %in% wanted_columns("taxa")]
  #     as.data.frame(result, stringsAsFactors = FALSE)
  #   }
  # ), fill = TRUE))
  # child_info
  
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