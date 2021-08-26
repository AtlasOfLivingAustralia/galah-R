#' Find valid taxonomic ranks
#' 
#' Return taxonomic ranks recognised by the ALA.
#' 
#' @export find_ranks
#' @return A \code{data.frame} of available ranks
#' @seealso This function provides a
#' reference that is useful when specifying the \code{down_to} argument of 
#' \code{\link{ala_taxonomy}}.
#' @examples \dontrun{
#' rank_df <- find_ranks()
#' }

find_ranks <- function() {
  data.frame(
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
}
