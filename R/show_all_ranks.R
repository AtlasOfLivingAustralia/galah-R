#' @rdname show_all_minifunctions
#' @export show_all_ranks
show_all_ranks <- function() {
  df <- data.frame(
    id = seq_len(69),
    name = c("root", "superkingdom", "kingdom", "subkingdom",
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
  )
  return(df |> as_tibble())
}

#' @rdname search_minifunctions
#' @export search_ranks
search_ranks <- function(query){
  df <- show_all_ranks()
  df[grepl(tolower(query), tolower(df$name)), ]
}