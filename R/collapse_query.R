#' Internal function to parse a `query_set` into a single `query`
#' @param x a `query_set`
#' @noRd
#' @keywords Internal
collapse_query <- function(x){
  switch(x$type,
         "data/occurrences-count-groupby" = collapse_occurrences_count(x),
         "data/occurrences-count" = collapse_occurrences_count(x),
         "data/species-count" = collapse_species_count(x),
         # "-unnest" functions require some checks
         "metadata/profiles-unnest" = collapse_profile_values(x),  # check this
         # some "metadata/" functions require pagination under some circumstances
         "metadata/lists" = collapse_lists(x), # always paginates
         x # remaining "metadata/" functions are passed as-is # fixme to make a new object type
  )
}