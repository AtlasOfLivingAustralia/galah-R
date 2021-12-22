#' Find valid taxonomic ranks
#' 
#' Return taxonomic ranks recognised by the ALA.
#' 
#' @export show_all_ranks
#' @return An object of class `tbl_df` and `data.frame` (aka a tibble) of 
#' available ranks
#' @seealso This function provides a
#' reference that is useful when specifying the `down_to` argument of 
#' [atlas_taxonomy()].
#' @examples
#' rank_df <- show_all_ranks()

show_all_ranks <- function() {
  local_check <- galah_internal()$show_all_ranks
  if(!is.null(local_check)){
    local_check
  }else{
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
    galah_internal(show_all_ranks = df)
    return(df |> as_tibble())
  }
}
