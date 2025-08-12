#' Internal function to parse a `query_set` into a single `query`
#' @param x a `query_set`
#' @noRd
#' @keywords Internal
collapse_query <- function(x){
  switch(x$type,
         "data/occurrences" = collapse_occurrences(x),
         "data/occurrences-count" = {
           if(is_gbif()){
             collapse_occurrences_count_gbif(x)
           }else{
             collapse_occurrences_count_atlas(x)   
           }
         },
         "data/occurrences-count-groupby" = {
           if(is_gbif()){
             if(nrow(x$body$group_by) > 1){
               collapse_occurrences_count_gbif_groupby_crossed(x)
             }else{
               collapse_occurrences_count_gbif_groupby_basic(x)
             }
           }else{
             collapse_occurrences_count_atlas_groupby(x)   
           }
         },
         "data/species" = collapse_occurrences(x), # optimised for GBIF
         "data/species-count" = collapse_species_count(x),
         # "-unnest" functions require some checks
         "metadata/profiles-unnest" = collapse_profile_values(x),  # check this
         # some "metadata/" functions require pagination under some circumstances
         "metadata/lists" = collapse_lists(x), # always paginates
         x # remaining "metadata/" functions are passed as-is # fixme to make a new object type
  )
}