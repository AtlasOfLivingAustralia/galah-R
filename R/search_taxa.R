#' Search taxon information
#' 
#' @export

search_taxa <- function(query, all_ranks = FALSE, downto = NULL){
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
    current_rank <- out_data$rank
    while (is_higher_rank(current_rank, downto)) {
      level_down(out_data$taxon_concept_id)
    }
  }
  
  if (ncol(out_data) > 1 && all_ranks) {
    im_ranks <- data.table::rbindlist(
      lapply(out_data$taxon_concept_id, function(id) {
        intermediate_ranks(id)
      }
      ), fill = TRUE)
    out_data <- cbind(out_data, im_ranks)
  }
    
  out_data
}

is_higher_rank <- function(rank1, rank2) {
  ranks_list <- c("root", "superkingdom", "kingdom", "subkingdom", 
                  "superphylum", "phylum", "subplylum", "superclass", "class", 
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
  
  if(which(ranks_list == rank1) < which(ranks_list == rank2)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

level_down <- function(identifier) {
  url <- server_config("species_base_url")
  path <- paste0(
    "ws/childConcepts/",
    URLencode(as.character(identifier), reserved = TRUE)
  )
  children <- ala_GET(url, path)
  if (length(children) == 0) {
    message("No child concepts found for taxon id \"", identifier, "\"")
    return()
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
  children
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